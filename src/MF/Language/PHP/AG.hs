

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
import Data.Graph.Inductive hiding (graphviz', Node, empty, nodes)
import Data.Graph.Inductive.Tree

import qualified MF.Core.Flowable as F
import MF.Core.Flowable hiding (Return)
{-# LINE 28 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/Simplify.ag" #-}

import Data.IntMap as IM
import CCO.Component

{-# LINE 35 "src/MF/Language/PHP/AG.hs" #-}

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
{-# LINE 52 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/Checking.ag" #-}

import CCO.Component
import MF.Core.Solver (mergeWith)
{-# LINE 58 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}

import Data.Graph.Inductive hiding (graphviz', Node, empty, nodes)
import Data.Graph.Inductive.Tree
{-# LINE 64 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}

import CCO.Printing as P hiding (render, join) 
{-# LINE 69 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}

import CCO.Printing as P hiding (render, join) 
import Data.IntMap as IM
{-# LINE 75 "src/MF/Language/PHP/AG.hs" #-}
{-# LINE 13 "src/MF/Language/PHP/AG.ag" #-}

execute mapping p = wrap_Node (sem_Node p) inh
    where
        inh = Inh_Node 
              { 
                  labels_Inh_Node = 0,
                  simplifiedName_Inh_Node = Nothing,
                  mapping_Inh_Node = mapping,
                  declarations'_Inh_Node = undefined,
                  declaration_Inh_Node = undefined,
                  struct_Inh_Node = undefined
              }
{-# LINE 89 "src/MF/Language/PHP/AG.hs" #-}

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

{-# LINE 189 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 145 "src/MF/Language/PHP/AG/Flow.ag" #-}

lookupDeclaration name declarations = case M.lookup name declarations of 
                                          Nothing   -> error $ "Calling an undefined function: " ++ name
                                          Just info -> info
{-# LINE 196 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}

data Declaration = Declaration { functionName :: String, ln :: Label, lx :: Label }
{-# LINE 201 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 192 "src/MF/Language/PHP/AG/Flow.ag" #-}


nextUnique :: Int -> (Int, Int)
nextUnique u = (u+1, u)

-- Make Node an instance of Flowable, in this way it can be consumed by a monotone framework
instance Flowable Node where
    init     = fromJust . init_Syn_Node . execute M.empty
    final    = fromJust . final_Syn_Node . execute M.empty
    flow     = flow_Syn_Node . execute M.empty
    blocks   = blocks_Syn_Node . execute M.empty

{-# LINE 216 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 210 "src/MF/Language/PHP/AG/Flow.ag" #-}


visualizecf p = trace ("nodes: " ++ show nodeList ++ ", edges: " ++ show edgeList ++ ", p: " ++ show p) $ graphvizz (flow p) (mkGraph nodeList edgeList :: Gr String ())
    where
        nodeList = nodeList_Syn_Node . execute M.empty $ p
        edgeList = edgeList_Syn_Node . execute M.empty $ p
        graphvizz f g = let n = labNodes g
                        --    e = labEdges g
                            
                            ns = concatMap sn n
                            es = concatMap se f
                            
                            sn (l, a)  = show l ++ " [label=\"" ++ a ++ " (" ++ show l ++ ") \"];"
                            se (l, l') = show l ++ " -> " ++ show l' ++ ";"
                      
                        in "digraph AST {" ++ ns ++ es ++ "}"
  
        
{-# LINE 237 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 21 "src/MF/Language/PHP/AG/Simplify.ag" #-}


buildExpect params = Expect expr (read $ "fromList" ++ value)
    where
        (Param expr)                     = params !! 0
        (Param (DQContent (Some value))) = params !! 1
        

annotator :: Component Node Node
annotator = component $ return . annotate

annotate = annotated_Syn_Node . execute M.empty

{-# LINE 253 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 123 "src/MF/Language/PHP/AG/Simplify.ag" #-}


buildVariable label = Variable $ Simple $ "#" ++ show label 

-- | Generic function to outline a node based on a mapping
-- outline :: (Label -> Node -> Node) -> Node -> IntMap Node -> Node
exstract build update node mapping = sequence $ buildSequence ++ [update node mapping]
    where
        buildSequence = IM.elems . IM.mapWithKey build $ mapping
        
-- | Outlines function calls, every function call is moved in front of the expression and replaced by a variable. The function calls itself are placed just before the 
-- | expression
exstractFunctions = exstract build update
    where
        build label (FunctionCall (FunctionName name) params) = SimplifiedFunctionCall name params $ Just (buildVariable label)
        update = const
        
            
-- | Simplifies a function call        
exstractParameters node mapping = exstract build update node filtered
    where
        build label expr = Expr $ Assign (buildVariable label) expr
        filtered = IM.filter (not . isVariable) mapping 
        
        isVariable :: Node -> Bool
        isVariable (Variable _) = True
        isVariable n            = False
        
        update (SimplifiedFunctionCall name params result) foo = SimplifiedFunctionCall name (IM.elems . IM.mapWithKey buildParam $ mapping) result
        
        buildParam label expr | isVariable expr = Param expr
                              | otherwise       = Param $ buildVariable label
        
        

simplify node = let a = simplified_Syn_Node . execute M.empty $ node
                    b = removed_Syn_Node . execute M.empty $ a
                    c = fixPoint (exstractFunctions_Syn_Node . execute M.empty) $ b
                    d = exstractParameters_Syn_Node . execute M.empty $ c
                in d


simplifier :: Component Node Node
simplifier = component $ return . simplify


{-# LINE 302 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 26 "src/MF/Language/PHP/AG/Typing.ag" #-}

levels :: Node -> Int
levels (ArrayAccess rv index) = 1 + levels rv
levels (Variable n)           = 0
      
name :: Node -> String
name   (ArrayAccess rv index) = name rv
name   (Variable n)           = name n
name   (Simple value)         = value
{-# LINE 314 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 97 "src/MF/Language/PHP/AG/Typing.ag" #-}


solve :: Node -> ValueMap (Stack :-> Mapping)
solve = C.solve (lift transfer) (M.singleton [] M.empty) M.empty Forward
    where        
        transfer :: Block Node -> Mapping -> Mapping
        transfer (Normal Skip)          = id
        transfer (Normal (Expect _ _))  = id
        transfer (Normal (Expr e))      = updateMapping e
        transfer (Normal (If c _ _ _ )) = updateMapping c
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
        updateMapping node mapping = mapping_Syn_Node . execute mapping $ node
                
        removeParam :: Node -> Node   
        removeParam (Param expr) = expr 

        toName :: Node -> String
        toName (Variable (Simple name)) = name
        
-------------------------------------------------------------------------------
-- Typing
-------------------------------------------------------------------------------
     
typer :: Component Node (ValueMap (Identifier :-> TypeSet))
typer = component $ return . typing
             
typing :: Node -> ValueMap (Identifier :-> TypeSet)
typing p = trace ("mappings: " ++ show mappings) $ mappings
    where
        solve = MF.Language.PHP.AG.solve
        mappings = IM.map (M.fold join M.empty) (solve p)        

-------------------------------------------------------------------------------
-- Report Typing
-------------------------------------------------------------------------------

reporterty :: Component (ValueMap (Identifier :-> TypeSet)) Doc
reporterty = component $ return . reporty     
             

reporty ::ValueMap (Identifier :-> TypeSet) -> Doc
reporty vm = IM.foldWithKey foldvm P.empty vm
            where foldvm i m r = text "Node num " >|< text (show i) >-< (M.foldrWithKey displayTypes P.empty m) >-< text "-----------" >-< r

displayTypes id ty r = text (show id) >|< text (show ty) >-< r

{-# LINE 382 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 11 "src/MF/Language/PHP/AG/Checking.ag" #-}
 
tyNum = S.fromList [TyInt, TyFloat] 
{-# LINE 387 "src/MF/Language/PHP/AG.hs" #-}

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
        mappings = IM.map (M.fold join M.empty) (solve p)
        nodes' = IM.map toNode (blocks p)
        
        findWarnings node mapping = trace ("finding warnings for " ++ show node ++ " with " ++ show mapping) $ warnings_Syn_Node . execute mapping $ node

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
        nodeList = nodeList_Syn_Node . execute M.empty $ p
        edgeList = edgeList_Syn_Node . execute M.empty $ p
        
        
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
    pp = pp_Syn_Node . execute M.empty

{-# LINE 512 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 66 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}


buildLabelRef :: IntMap String -> Label -> String
buildLabelRef imap l = case IM.lookup l imap of
                            Just s -> s ++ ":" ++ (show l)
                            Nothing -> error $ show imap ++ show l

cfgprinter :: Component Node Doc
cfgprinter = component $ return . cfgprint
             
cfgprint = ppcfg_Syn_Node . execute M.empty


{-# LINE 528 "src/MF/Language/PHP/AG.hs" #-}
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
type T_Node  = Declaration ->
               (Map String Declaration) ->
               Label ->
               Mapping ->
               (Maybe Node) ->
               String ->
               ( Node ,(IntMap (Block Node)),(IntMap Node),(Set Constraint),(Map String Declaration),([UEdge]),([UEdge]),(Set Constraint),Node ,Node ,(Maybe [Label]),Flow,Doc,(Maybe Label),Label,Label,(IntMap String),Mapping,([LNode String]),([LNode String]),(IntMap Node),(IntMap Node),Doc,Doc,Node ,Node ,Node ,(Set Warning))
data Inh_Node  = Inh_Node {declaration_Inh_Node :: Declaration,declarations'_Inh_Node :: (Map String Declaration),labels_Inh_Node :: Label,mapping_Inh_Node :: Mapping,simplifiedName_Inh_Node :: (Maybe Node),struct_Inh_Node :: String}
data Syn_Node  = Syn_Node {annotated_Syn_Node :: Node ,blocks_Syn_Node :: (IntMap (Block Node)),callMapping_Syn_Node :: (IntMap Node),constraints_Syn_Node :: (Set Constraint),declarations_Syn_Node :: (Map String Declaration),edgeList_Syn_Node :: ([UEdge]),edgeListflow_Syn_Node :: ([UEdge]),expected_Syn_Node :: (Set Constraint),exstractFunctions_Syn_Node :: Node ,exstractParameters_Syn_Node :: Node ,final_Syn_Node :: (Maybe [Label]),flow_Syn_Node :: Flow,flowpp_Syn_Node :: Doc,init_Syn_Node :: (Maybe Label),label_Syn_Node :: Label,labels_Syn_Node :: Label,labstruct_Syn_Node :: (IntMap String),mapping_Syn_Node :: Mapping,nodeList_Syn_Node :: ([LNode String]),nodeListflow_Syn_Node :: ([LNode String]),nodes_Syn_Node :: (IntMap Node),paramMapping_Syn_Node :: (IntMap Node),pp_Syn_Node :: Doc,ppcfg_Syn_Node :: Doc,removed_Syn_Node :: Node ,self_Syn_Node :: Node ,simplified_Syn_Node :: Node ,warnings_Syn_Node :: (Set Warning)}
wrap_Node :: T_Node  ->
             Inh_Node  ->
             Syn_Node 
wrap_Node sem (Inh_Node _lhsIdeclaration _lhsIdeclarations' _lhsIlabels _lhsImapping _lhsIsimplifiedName _lhsIstruct )  =
    (let ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings) = sem _lhsIdeclaration _lhsIdeclarations' _lhsIlabels _lhsImapping _lhsIsimplifiedName _lhsIstruct 
     in  (Syn_Node _lhsOannotated _lhsOblocks _lhsOcallMapping _lhsOconstraints _lhsOdeclarations _lhsOedgeList _lhsOedgeListflow _lhsOexpected _lhsOexstractFunctions _lhsOexstractParameters _lhsOfinal _lhsOflow _lhsOflowpp _lhsOinit _lhsOlabel _lhsOlabels _lhsOlabstruct _lhsOmapping _lhsOnodeList _lhsOnodeListflow _lhsOnodes _lhsOparamMapping _lhsOpp _lhsOppcfg _lhsOremoved _lhsOself _lhsOsimplified _lhsOwarnings ))
sem_Node_ArrayAccess :: T_Node  ->
                        T_Node  ->
                        T_Node 
sem_Node_ArrayAccess rv_ index_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _rvOdeclaration :: Declaration
              _rvOdeclarations' :: (Map String Declaration)
              _rvOmapping :: Mapping
              _rvOstruct :: String
              _indexOdeclaration :: Declaration
              _indexOdeclarations' :: (Map String Declaration)
              _indexOlabels :: Label
              _indexOmapping :: Mapping
              _indexOsimplifiedName :: (Maybe Node)
              _indexOstruct :: String
              _rvIannotated :: Node 
              _rvIblocks :: (IntMap (Block Node))
              _rvIcallMapping :: (IntMap Node)
              _rvIconstraints :: (Set Constraint)
              _rvIdeclarations :: (Map String Declaration)
              _rvIedgeList :: ([UEdge])
              _rvIedgeListflow :: ([UEdge])
              _rvIexpected :: (Set Constraint)
              _rvIexstractFunctions :: Node 
              _rvIexstractParameters :: Node 
              _rvIfinal :: (Maybe [Label])
              _rvIflow :: Flow
              _rvIflowpp :: Doc
              _rvIinit :: (Maybe Label)
              _rvIlabel :: Label
              _rvIlabels :: Label
              _rvIlabstruct :: (IntMap String)
              _rvImapping :: Mapping
              _rvInodeList :: ([LNode String])
              _rvInodeListflow :: ([LNode String])
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
              _indexIedgeListflow :: ([UEdge])
              _indexIexpected :: (Set Constraint)
              _indexIexstractFunctions :: Node 
              _indexIexstractParameters :: Node 
              _indexIfinal :: (Maybe [Label])
              _indexIflow :: Flow
              _indexIflowpp :: Doc
              _indexIinit :: (Maybe Label)
              _indexIlabel :: Label
              _indexIlabels :: Label
              _indexIlabstruct :: (IntMap String)
              _indexImapping :: Mapping
              _indexInodeList :: ([LNode String])
              _indexInodeListflow :: ([LNode String])
              _indexInodes :: (IntMap Node)
              _indexIparamMapping :: (IntMap Node)
              _indexIpp :: Doc
              _indexIppcfg :: Doc
              _indexIremoved :: Node 
              _indexIself :: Node 
              _indexIsimplified :: Node 
              _indexIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 772 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :<=: _rvIlabel)
                   {-# LINE 777 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 782 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 787 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 792 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOsimplifiedName =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName <|> pure _self
                   {-# LINE 797 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _rvIpp >|< text "[" >|< _indexIpp >|< text "]"
                   {-# LINE 802 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup1 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_rvOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 809 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 814 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIblocks `IM.union` _indexIblocks
                   {-# LINE 819 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIcallMapping `IM.union` _indexIcallMapping
                   {-# LINE 824 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvIconstraints `S.union` _indexIconstraints
                   {-# LINE 829 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIdeclarations `M.union` _indexIdeclarations
                   {-# LINE 834 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _rvIedgeList ++ _indexIedgeList
                   {-# LINE 839 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIedgeListflow ++ _indexIedgeListflow
                   {-# LINE 844 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIexpected `S.union` _indexIexpected
                   {-# LINE 849 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIfinal <|> _indexIfinal
                   {-# LINE 854 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIflow ++ _indexIflow
                   {-# LINE 859 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIflowpp >|< _indexIflowpp
                   {-# LINE 864 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIinit <|> _indexIinit
                   {-# LINE 869 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIlabstruct `IM.union` _indexIlabstruct
                   {-# LINE 874 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _rvInodeList ++ _indexInodeList
                   {-# LINE 879 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvInodeListflow ++ _indexInodeListflow
                   {-# LINE 884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvInodes `IM.union` _indexInodes
                   {-# LINE 889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIparamMapping `IM.union` _indexIparamMapping
                   {-# LINE 894 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIppcfg >|< _indexIppcfg
                   {-# LINE 899 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIwarnings `S.union` _indexIwarnings
                   {-# LINE 904 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIannotated _indexIannotated
                   {-# LINE 909 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIexstractFunctions _indexIexstractFunctions
                   {-# LINE 914 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIexstractParameters _indexIexstractParameters
                   {-# LINE 919 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIremoved _indexIremoved
                   {-# LINE 924 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ArrayAccess _rvIself _indexIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIsimplified _indexIsimplified
                   {-# LINE 931 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 936 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 941 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 946 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 951 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 958 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _indexIlabels
                   {-# LINE 963 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _indexImapping
                   {-# LINE 968 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 973 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 978 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 983 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 988 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 993 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 998 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIlabels
                   {-# LINE 1003 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvImapping
                   {-# LINE 1008 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1013 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1018 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _rvIannotated,_rvIblocks,_rvIcallMapping,_rvIconstraints,_rvIdeclarations,_rvIedgeList,_rvIedgeListflow,_rvIexpected,_rvIexstractFunctions,_rvIexstractParameters,_rvIfinal,_rvIflow,_rvIflowpp,_rvIinit,_rvIlabel,_rvIlabels,_rvIlabstruct,_rvImapping,_rvInodeList,_rvInodeListflow,_rvInodes,_rvIparamMapping,_rvIpp,_rvIppcfg,_rvIremoved,_rvIself,_rvIsimplified,_rvIwarnings) =
                  rv_ _rvOdeclaration _rvOdeclarations' _rvOlabels _rvOmapping _rvOsimplifiedName _rvOstruct 
              ( _indexIannotated,_indexIblocks,_indexIcallMapping,_indexIconstraints,_indexIdeclarations,_indexIedgeList,_indexIedgeListflow,_indexIexpected,_indexIexstractFunctions,_indexIexstractParameters,_indexIfinal,_indexIflow,_indexIflowpp,_indexIinit,_indexIlabel,_indexIlabels,_indexIlabstruct,_indexImapping,_indexInodeList,_indexInodeListflow,_indexInodes,_indexIparamMapping,_indexIpp,_indexIppcfg,_indexIremoved,_indexIself,_indexIsimplified,_indexIwarnings) =
                  index_ _indexOdeclaration _indexOdeclarations' _indexOlabels _indexOmapping _indexOsimplifiedName _indexOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Assign :: T_Node  ->
                   T_Node  ->
                   T_Node 
sem_Node_Assign rv_ e_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOflow :: Flow
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOconstraints :: (Set Constraint)
              _lhsOmapping :: Mapping
              _lhsOnodeList :: ([LNode String])
              _lhsOedgeList :: ([UEdge])
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOflowpp :: Doc
              _rvOstruct :: String
              _eOstruct :: String
              __tup2 :: ((Label,Label))
              _rvOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _rvOdeclaration :: Declaration
              _rvOdeclarations' :: (Map String Declaration)
              _rvOmapping :: Mapping
              _rvOsimplifiedName :: (Maybe Node)
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOlabels :: Label
              _eOmapping :: Mapping
              _eOsimplifiedName :: (Maybe Node)
              _rvIannotated :: Node 
              _rvIblocks :: (IntMap (Block Node))
              _rvIcallMapping :: (IntMap Node)
              _rvIconstraints :: (Set Constraint)
              _rvIdeclarations :: (Map String Declaration)
              _rvIedgeList :: ([UEdge])
              _rvIedgeListflow :: ([UEdge])
              _rvIexpected :: (Set Constraint)
              _rvIexstractFunctions :: Node 
              _rvIexstractParameters :: Node 
              _rvIfinal :: (Maybe [Label])
              _rvIflow :: Flow
              _rvIflowpp :: Doc
              _rvIinit :: (Maybe Label)
              _rvIlabel :: Label
              _rvIlabels :: Label
              _rvIlabstruct :: (IntMap String)
              _rvImapping :: Mapping
              _rvInodeList :: ([LNode String])
              _rvInodeListflow :: ([LNode String])
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
              _eIedgeListflow :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIflowpp :: Doc
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodeListflow :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _lhsOflow =
                  ({-# LINE 142 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 1136 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 142 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) _flow
                   {-# LINE 1141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 1146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union _lstr
                   {-# LINE 1151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 1156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 1161 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 1166 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 1171 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 101 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 1176 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow =
                  ({-# LINE 143 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   [(l', fromJust _rvIinit) | l' <- fromJust _eIfinal] ++ [(l', _label) | l' <- fromJust _rvIfinal]
                   {-# LINE 1181 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 1186 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.fromList [(_rvIlabel :<=: _eIlabel), (_label :<=: _rvIlabel)] `S.union` _eIconstraints
                   {-# LINE 1191 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 89 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   updateMapping (Identifier $ name _rvIself) _eIlabel (levels _rvIself) _constraints _eImapping
                   {-# LINE 1196 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 1201 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, "="), (_rvIlabel, render _rvIpp)] ++ _eInodeList
                   {-# LINE 1206 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, _rvIlabel, ()), (_label, _eIlabel, ())] ++ _eIedgeList
                   {-# LINE 1211 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _rvIpp >|< text " = " >|< _eIpp
                   {-# LINE 1216 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIppcfg >|<
                   text " | <" >|< text (show _label) >|< text "> = | " >|<
                   _eIppcfg
                   {-# LINE 1223 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   above [text (buildLabelRef _lstr     l) >|< text "-> " >|< text (buildLabelRef _lstr     l') >|< text ";" | (l,l') <- _flow    ]
                   {-# LINE 1228 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOstruct =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1233 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1238 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lstr =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIlabstruct `IM.union` _eIlabstruct `IM.union` (IM.singleton _label _lhsIstruct)
                   {-# LINE 1243 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup2 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_rvOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 1250 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 1255 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIblocks `IM.union` _eIblocks
                   {-# LINE 1260 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIcallMapping `IM.union` _eIcallMapping
                   {-# LINE 1265 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIdeclarations `M.union` _eIdeclarations
                   {-# LINE 1270 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIedgeListflow ++ _eIedgeListflow
                   {-# LINE 1275 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 1280 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 142 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _flow
                   {-# LINE 1285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIlabstruct `IM.union` _eIlabstruct
                   {-# LINE 1290 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvInodeListflow ++ _eInodeListflow
                   {-# LINE 1295 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvInodes `IM.union` _eInodes
                   {-# LINE 1300 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIparamMapping `IM.union` _eIparamMapping
                   {-# LINE 1305 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIwarnings `S.union` _eIwarnings
                   {-# LINE 1310 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIannotated _eIannotated
                   {-# LINE 1315 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIexstractFunctions _eIexstractFunctions
                   {-# LINE 1320 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIexstractParameters _eIexstractParameters
                   {-# LINE 1325 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIremoved _eIremoved
                   {-# LINE 1330 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Assign _rvIself _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIsimplified _eIsimplified
                   {-# LINE 1337 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 1342 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 1347 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 1352 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1357 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1364 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 1369 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1374 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1379 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1384 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1389 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1394 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1399 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIlabels
                   {-# LINE 1404 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvImapping
                   {-# LINE 1409 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1414 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _rvIannotated,_rvIblocks,_rvIcallMapping,_rvIconstraints,_rvIdeclarations,_rvIedgeList,_rvIedgeListflow,_rvIexpected,_rvIexstractFunctions,_rvIexstractParameters,_rvIfinal,_rvIflow,_rvIflowpp,_rvIinit,_rvIlabel,_rvIlabels,_rvIlabstruct,_rvImapping,_rvInodeList,_rvInodeListflow,_rvInodes,_rvIparamMapping,_rvIpp,_rvIppcfg,_rvIremoved,_rvIself,_rvIsimplified,_rvIwarnings) =
                  rv_ _rvOdeclaration _rvOdeclarations' _rvOlabels _rvOmapping _rvOsimplifiedName _rvOstruct 
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIedgeListflow,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIflowpp,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodeListflow,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName _eOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Block :: T_Node  ->
                  T_Node 
sem_Node_Block s_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOsimplifiedName :: (Maybe Node)
              _sOstruct :: String
              _sIannotated :: Node 
              _sIblocks :: (IntMap (Block Node))
              _sIcallMapping :: (IntMap Node)
              _sIconstraints :: (Set Constraint)
              _sIdeclarations :: (Map String Declaration)
              _sIedgeList :: ([UEdge])
              _sIedgeListflow :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIexstractFunctions :: Node 
              _sIexstractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIflowpp :: Doc
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sIlabstruct :: (IntMap String)
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodeListflow :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIppcfg :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIwarnings :: (Set Warning)
              _lhsOinit =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIinit
                   {-# LINE 1495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 111 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIfinal
                   {-# LINE 1500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIblocks
                   {-# LINE 1505 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _sIcallMapping
                   {-# LINE 1510 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 1515 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIdeclarations
                   {-# LINE 1520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _sIedgeList
                   {-# LINE 1525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIedgeListflow
                   {-# LINE 1530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _sIexpected
                   {-# LINE 1535 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIflow
                   {-# LINE 1540 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sIflowpp
                   {-# LINE 1545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sIlabstruct
                   {-# LINE 1550 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _sInodeList
                   {-# LINE 1555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sInodeListflow
                   {-# LINE 1560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sInodes
                   {-# LINE 1565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _sIparamMapping
                   {-# LINE 1570 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _sIpp
                   {-# LINE 1575 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sIppcfg
                   {-# LINE 1580 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _sIwarnings
                   {-# LINE 1585 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIannotated
                   {-# LINE 1590 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIexstractFunctions
                   {-# LINE 1595 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIexstractParameters
                   {-# LINE 1600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIremoved
                   {-# LINE 1605 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Block _sIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIsimplified
                   {-# LINE 1612 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 1617 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 1622 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 1627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1632 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1639 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabel
                   {-# LINE 1644 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 1649 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 1654 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1659 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1664 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 1669 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1674 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1679 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1684 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIdeclarations,_sIedgeList,_sIedgeListflow,_sIexpected,_sIexstractFunctions,_sIexstractParameters,_sIfinal,_sIflow,_sIflowpp,_sIinit,_sIlabel,_sIlabels,_sIlabstruct,_sImapping,_sInodeList,_sInodeListflow,_sInodes,_sIparamMapping,_sIpp,_sIppcfg,_sIremoved,_sIself,_sIsimplified,_sIwarnings) =
                  s_ _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOsimplifiedName _sOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_CloseTag :: T_Node 
sem_Node_CloseTag  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup3 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 1730 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 1735 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 1740 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup3 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 1747 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 1752 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 1757 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 1762 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 1767 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 1772 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 1777 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 1782 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 1787 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 1792 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 1797 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 1802 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 1807 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 1812 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 1817 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 1822 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 1827 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 1832 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 1837 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 1842 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 1847 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1852 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1857 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1862 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1867 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  CloseTag
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1874 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 1879 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 1884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 1889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1894 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1901 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1906 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_ConstantEncapsedString :: T_Node  ->
                                   T_Node 
sem_Node_ConstantEncapsedString n_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOpp :: Doc
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _nOdeclaration :: Declaration
              _nOdeclarations' :: (Map String Declaration)
              _nOlabels :: Label
              _nOmapping :: Mapping
              _nOsimplifiedName :: (Maybe Node)
              _nOstruct :: String
              _nIannotated :: Node 
              _nIblocks :: (IntMap (Block Node))
              _nIcallMapping :: (IntMap Node)
              _nIconstraints :: (Set Constraint)
              _nIdeclarations :: (Map String Declaration)
              _nIedgeList :: ([UEdge])
              _nIedgeListflow :: ([UEdge])
              _nIexpected :: (Set Constraint)
              _nIexstractFunctions :: Node 
              _nIexstractParameters :: Node 
              _nIfinal :: (Maybe [Label])
              _nIflow :: Flow
              _nIflowpp :: Doc
              _nIinit :: (Maybe Label)
              _nIlabel :: Label
              _nIlabels :: Label
              _nIlabstruct :: (IntMap String)
              _nImapping :: Mapping
              _nInodeList :: ([LNode String])
              _nInodeListflow :: ([LNode String])
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
                   {-# LINE 1983 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 1988 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 1993 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 1998 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 2003 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nIedgeList
                   {-# LINE 2008 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIedgeListflow
                   {-# LINE 2013 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 2018 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIfinal
                   {-# LINE 2023 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 2028 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIflowpp
                   {-# LINE 2033 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIinit
                   {-# LINE 2038 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIlabstruct
                   {-# LINE 2043 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nInodeList
                   {-# LINE 2048 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nInodeListflow
                   {-# LINE 2053 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 2058 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 2063 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIppcfg
                   {-# LINE 2068 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 2073 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIannotated
                   {-# LINE 2078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIexstractFunctions
                   {-# LINE 2083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIexstractParameters
                   {-# LINE 2088 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIremoved
                   {-# LINE 2093 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ConstantEncapsedString _nIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIsimplified
                   {-# LINE 2100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 2105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 2110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 2115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2120 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2127 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabel
                   {-# LINE 2132 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 2137 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 2142 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 2147 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 2152 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 2157 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2162 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 2167 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 2172 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _nIannotated,_nIblocks,_nIcallMapping,_nIconstraints,_nIdeclarations,_nIedgeList,_nIedgeListflow,_nIexpected,_nIexstractFunctions,_nIexstractParameters,_nIfinal,_nIflow,_nIflowpp,_nIinit,_nIlabel,_nIlabels,_nIlabstruct,_nImapping,_nInodeList,_nInodeListflow,_nInodes,_nIparamMapping,_nIpp,_nIppcfg,_nIremoved,_nIself,_nIsimplified,_nIwarnings) =
                  n_ _nOdeclaration _nOdeclarations' _nOlabels _nOmapping _nOsimplifiedName _nOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_DQContent :: T_OptionalString  ->
                      T_Node 
sem_Node_DQContent value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOmapping :: Mapping
              _valueIannotated :: OptionalString 
              _valueIexstractFunctions :: OptionalString 
              _valueIexstractParameters :: OptionalString 
              _valueIparamMapping :: (IntMap Node)
              _valueIpp :: Doc
              _valueIremoved :: OptionalString 
              _valueIself :: OptionalString 
              _valueIsimplified :: OptionalString 
              _valueIvalue :: String
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 2228 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2233 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2238 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOsimplified =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String _valueIvalue
                   {-# LINE 2243 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _valueIpp
                   {-# LINE 2248 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup4 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 2255 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 2260 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2265 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2270 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 2275 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2280 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2290 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2295 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2300 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2305 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 2310 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2315 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 2320 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2325 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2330 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2335 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _valueIparamMapping
                   {-# LINE 2340 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 2345 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2350 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIannotated
                   {-# LINE 2355 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIexstractFunctions
                   {-# LINE 2360 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIexstractParameters
                   {-# LINE 2365 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIremoved
                   {-# LINE 2370 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  DQContent _valueIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIsimplified
                   {-# LINE 2377 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 2382 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 2387 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 2392 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2397 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2404 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _valueIannotated,_valueIexstractFunctions,_valueIexstractParameters,_valueIparamMapping,_valueIpp,_valueIremoved,_valueIself,_valueIsimplified,_valueIvalue) =
                  value_ 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Deci :: Integer ->
                 T_Node 
sem_Node_Deci value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOlabstruct :: (IntMap String)
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
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOlabstruct =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 2451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
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
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 2471 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2476 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2481 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 2486 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 105 "src/MF/Language/PHP/AG/Flow.ag" #-}
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
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text "<" >|< text (show _label) >|< text "> " >|< text (show value_)
                   {-# LINE 2511 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup5 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 2518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 2523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2548 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2553 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 2558 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 2563 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2568 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2573 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2578 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2583 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2588 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2593 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2598 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2603 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2608 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Deci value_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2615 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 2620 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 2625 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 2630 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2635 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2647 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Document :: ([Node]) ->
                     T_Node  ->
                     T_Node  ->
                     T_Node  ->
                     ([Node]) ->
                     T_Node 
sem_Node_Document before_ opentag_ stmt_ closetag_ after_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _stmtOdeclarations' :: (Map String Declaration)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _opentagOdeclaration :: Declaration
              _opentagOdeclarations' :: (Map String Declaration)
              _opentagOlabels :: Label
              _opentagOmapping :: Mapping
              _opentagOsimplifiedName :: (Maybe Node)
              _opentagOstruct :: String
              _stmtOdeclaration :: Declaration
              _stmtOlabels :: Label
              _stmtOmapping :: Mapping
              _stmtOsimplifiedName :: (Maybe Node)
              _stmtOstruct :: String
              _closetagOdeclaration :: Declaration
              _closetagOdeclarations' :: (Map String Declaration)
              _closetagOlabels :: Label
              _closetagOmapping :: Mapping
              _closetagOsimplifiedName :: (Maybe Node)
              _closetagOstruct :: String
              _opentagIannotated :: Node 
              _opentagIblocks :: (IntMap (Block Node))
              _opentagIcallMapping :: (IntMap Node)
              _opentagIconstraints :: (Set Constraint)
              _opentagIdeclarations :: (Map String Declaration)
              _opentagIedgeList :: ([UEdge])
              _opentagIedgeListflow :: ([UEdge])
              _opentagIexpected :: (Set Constraint)
              _opentagIexstractFunctions :: Node 
              _opentagIexstractParameters :: Node 
              _opentagIfinal :: (Maybe [Label])
              _opentagIflow :: Flow
              _opentagIflowpp :: Doc
              _opentagIinit :: (Maybe Label)
              _opentagIlabel :: Label
              _opentagIlabels :: Label
              _opentagIlabstruct :: (IntMap String)
              _opentagImapping :: Mapping
              _opentagInodeList :: ([LNode String])
              _opentagInodeListflow :: ([LNode String])
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
              _stmtIedgeListflow :: ([UEdge])
              _stmtIexpected :: (Set Constraint)
              _stmtIexstractFunctions :: Node 
              _stmtIexstractParameters :: Node 
              _stmtIfinal :: (Maybe [Label])
              _stmtIflow :: Flow
              _stmtIflowpp :: Doc
              _stmtIinit :: (Maybe Label)
              _stmtIlabel :: Label
              _stmtIlabels :: Label
              _stmtIlabstruct :: (IntMap String)
              _stmtImapping :: Mapping
              _stmtInodeList :: ([LNode String])
              _stmtInodeListflow :: ([LNode String])
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
              _closetagIedgeListflow :: ([UEdge])
              _closetagIexpected :: (Set Constraint)
              _closetagIexstractFunctions :: Node 
              _closetagIexstractParameters :: Node 
              _closetagIfinal :: (Maybe [Label])
              _closetagIflow :: Flow
              _closetagIflowpp :: Doc
              _closetagIinit :: (Maybe Label)
              _closetagIlabel :: Label
              _closetagIlabels :: Label
              _closetagIlabstruct :: (IntMap String)
              _closetagImapping :: Mapping
              _closetagInodeList :: ([LNode String])
              _closetagInodeListflow :: ([LNode String])
              _closetagInodes :: (IntMap Node)
              _closetagIparamMapping :: (IntMap Node)
              _closetagIpp :: Doc
              _closetagIppcfg :: Doc
              _closetagIremoved :: Node 
              _closetagIself :: Node 
              _closetagIsimplified :: Node 
              _closetagIwarnings :: (Set Warning)
              _init =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIinit
                   {-# LINE 2796 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _init
                   {-# LINE 2801 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _final =
                  ({-# LINE 92 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIfinal
                   {-# LINE 2806 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 93 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _final
                   {-# LINE 2811 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclarations' =
                  ({-# LINE 167 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIdeclarations
                   {-# LINE 2816 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "<?" >-< _stmtIpp >-< text "?>"
                   {-# LINE 2821 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text "digraph structs {" >-<
                   text "node [shape=Mrecord];" >-<
                   text "init [label=\"init\", shape=circle]" >-<
                   text "final [label=\"final\", shape=circle, style=filled, fillcolor=gray]" >-<
                   _stmtIppcfg >-<
                   _flowp     >-<
                   text "}"
                   {-# LINE 2832 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flowp =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text "init -> " >|< text (buildLabelRef _stmtIlabstruct (fromJust _init)) >|< text ";" >-<
                   above [text (buildLabelRef _stmtIlabstruct f) >|< text "-> final;" | f <- fromJust _final] >-<
                   _stmtIflowpp
                   {-# LINE 2839 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIblocks `IM.union` _stmtIblocks `IM.union` _closetagIblocks
                   {-# LINE 2844 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _opentagIcallMapping `IM.union` _stmtIcallMapping `IM.union` _closetagIcallMapping
                   {-# LINE 2849 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _opentagIconstraints `S.union` _stmtIconstraints `S.union` _closetagIconstraints
                   {-# LINE 2854 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIdeclarations `M.union` _stmtIdeclarations `M.union` _closetagIdeclarations
                   {-# LINE 2859 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _opentagIedgeList ++ _stmtIedgeList ++ _closetagIedgeList
                   {-# LINE 2864 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIedgeListflow ++ _stmtIedgeListflow ++ _closetagIedgeListflow
                   {-# LINE 2869 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _opentagIexpected `S.union` _stmtIexpected `S.union` _closetagIexpected
                   {-# LINE 2874 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIflow ++ _stmtIflow ++ _closetagIflow
                   {-# LINE 2879 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _opentagIflowpp >|< _stmtIflowpp >|< _closetagIflowpp
                   {-# LINE 2884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _opentagIlabstruct `IM.union` _stmtIlabstruct `IM.union` _closetagIlabstruct
                   {-# LINE 2889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _opentagInodeList ++ _stmtInodeList ++ _closetagInodeList
                   {-# LINE 2894 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagInodeListflow ++ _stmtInodeListflow ++ _closetagInodeListflow
                   {-# LINE 2899 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagInodes `IM.union` _stmtInodes `IM.union` _closetagInodes
                   {-# LINE 2904 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _opentagIparamMapping `IM.union` _stmtIparamMapping `IM.union` _closetagIparamMapping
                   {-# LINE 2909 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _opentagIwarnings `S.union` _stmtIwarnings `S.union` _closetagIwarnings
                   {-# LINE 2914 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIannotated _stmtIannotated _closetagIannotated after_
                   {-# LINE 2919 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIexstractFunctions _stmtIexstractFunctions _closetagIexstractFunctions after_
                   {-# LINE 2924 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIexstractParameters _stmtIexstractParameters _closetagIexstractParameters after_
                   {-# LINE 2929 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIremoved _stmtIremoved _closetagIremoved after_
                   {-# LINE 2934 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Document before_ _opentagIself _stmtIself _closetagIself after_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIsimplified _stmtIsimplified _closetagIsimplified after_
                   {-# LINE 2941 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 2946 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 2951 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 2956 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2961 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2968 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _closetagIlabel
                   {-# LINE 2973 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _closetagIlabels
                   {-# LINE 2978 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _closetagImapping
                   {-# LINE 2983 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 2988 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 2993 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 2998 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3003 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3008 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3013 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3018 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIlabels
                   {-# LINE 3023 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _opentagImapping
                   {-# LINE 3028 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3033 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3038 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3043 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3048 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIlabels
                   {-# LINE 3053 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtImapping
                   {-# LINE 3058 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3063 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3068 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _opentagIannotated,_opentagIblocks,_opentagIcallMapping,_opentagIconstraints,_opentagIdeclarations,_opentagIedgeList,_opentagIedgeListflow,_opentagIexpected,_opentagIexstractFunctions,_opentagIexstractParameters,_opentagIfinal,_opentagIflow,_opentagIflowpp,_opentagIinit,_opentagIlabel,_opentagIlabels,_opentagIlabstruct,_opentagImapping,_opentagInodeList,_opentagInodeListflow,_opentagInodes,_opentagIparamMapping,_opentagIpp,_opentagIppcfg,_opentagIremoved,_opentagIself,_opentagIsimplified,_opentagIwarnings) =
                  opentag_ _opentagOdeclaration _opentagOdeclarations' _opentagOlabels _opentagOmapping _opentagOsimplifiedName _opentagOstruct 
              ( _stmtIannotated,_stmtIblocks,_stmtIcallMapping,_stmtIconstraints,_stmtIdeclarations,_stmtIedgeList,_stmtIedgeListflow,_stmtIexpected,_stmtIexstractFunctions,_stmtIexstractParameters,_stmtIfinal,_stmtIflow,_stmtIflowpp,_stmtIinit,_stmtIlabel,_stmtIlabels,_stmtIlabstruct,_stmtImapping,_stmtInodeList,_stmtInodeListflow,_stmtInodes,_stmtIparamMapping,_stmtIpp,_stmtIppcfg,_stmtIremoved,_stmtIself,_stmtIsimplified,_stmtIwarnings) =
                  stmt_ _stmtOdeclaration _stmtOdeclarations' _stmtOlabels _stmtOmapping _stmtOsimplifiedName _stmtOstruct 
              ( _closetagIannotated,_closetagIblocks,_closetagIcallMapping,_closetagIconstraints,_closetagIdeclarations,_closetagIedgeList,_closetagIedgeListflow,_closetagIexpected,_closetagIexstractFunctions,_closetagIexstractParameters,_closetagIfinal,_closetagIflow,_closetagIflowpp,_closetagIinit,_closetagIlabel,_closetagIlabels,_closetagIlabstruct,_closetagImapping,_closetagInodeList,_closetagInodeListflow,_closetagInodes,_closetagIparamMapping,_closetagIpp,_closetagIppcfg,_closetagIremoved,_closetagIself,_closetagIsimplified,_closetagIwarnings) =
                  closetag_ _closetagOdeclaration _closetagOdeclarations' _closetagOlabels _closetagOmapping _closetagOsimplifiedName _closetagOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Echo :: T_ParamList  ->
                 T_Node 
sem_Node_Echo e_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOsimplifiedName :: (Maybe Node)
              _eIannotated :: ParamList 
              _eIcallMapping :: (IntMap Node)
              _eIdeclarations :: (Map String Declaration)
              _eIexstractFunctions :: ParamList 
              _eIexstractParameters :: ParamList 
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
                   {-# LINE 3138 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "echo")]
                   {-# LINE 3143 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 3148 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 3153 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 3158 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 113 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "echo " >|< _eIpp
                   {-# LINE 3163 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup6 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 3170 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 3175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 3180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 3185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 3190 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 3195 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 3200 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 3205 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 3210 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 3215 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 3220 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 3225 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 3230 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 3235 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 3240 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 3245 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 3250 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 3255 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 3260 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 3265 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIannotated
                   {-# LINE 3270 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIexstractFunctions
                   {-# LINE 3275 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIexstractParameters
                   {-# LINE 3280 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIremoved
                   {-# LINE 3285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Echo _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIsimplified
                   {-# LINE 3292 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 3297 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 3302 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 3307 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3312 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3319 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 3324 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 3329 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3334 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3339 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3344 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3349 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIcallMapping,_eIdeclarations,_eIexstractFunctions,_eIexstractParameters,_eIlabel,_eIlabels,_eImapping,_eInodes,_eIparamMapping,_eIpp,_eIremoved,_eIself,_eIsimplified) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_ElseIf :: T_Node  ->
                   T_Node  ->
                   T_Node 
sem_Node_ElseIf e_ s_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOlabels :: Label
              _eOmapping :: Mapping
              _eOsimplifiedName :: (Maybe Node)
              _eOstruct :: String
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOsimplifiedName :: (Maybe Node)
              _sOstruct :: String
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIedgeListflow :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIflowpp :: Doc
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodeListflow :: ([LNode String])
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
              _sIedgeListflow :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIexstractFunctions :: Node 
              _sIexstractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIflowpp :: Doc
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sIlabstruct :: (IntMap String)
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodeListflow :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIppcfg :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks `IM.union` _sIblocks
                   {-# LINE 3463 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 3468 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints `S.union` _sIconstraints
                   {-# LINE 3473 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations `M.union` _sIdeclarations
                   {-# LINE 3478 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList ++ _sIedgeList
                   {-# LINE 3483 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIedgeListflow ++ _sIedgeListflow
                   {-# LINE 3488 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected `S.union` _sIexpected
                   {-# LINE 3493 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal <|> _sIfinal
                   {-# LINE 3498 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow ++ _sIflow
                   {-# LINE 3503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIflowpp >|< _sIflowpp
                   {-# LINE 3508 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit <|> _sIinit
                   {-# LINE 3513 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 3518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList ++ _sInodeList
                   {-# LINE 3523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodeListflow ++ _sInodeListflow
                   {-# LINE 3528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes `IM.union` _sInodes
                   {-# LINE 3533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 3538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp >|< _sIpp
                   {-# LINE 3543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg >|< _sIppcfg
                   {-# LINE 3548 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings `S.union` _sIwarnings
                   {-# LINE 3553 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIannotated _sIannotated
                   {-# LINE 3558 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIexstractFunctions _sIexstractFunctions
                   {-# LINE 3563 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIexstractParameters _sIexstractParameters
                   {-# LINE 3568 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIremoved _sIremoved
                   {-# LINE 3573 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ElseIf _eIself _sIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIsimplified _sIsimplified
                   {-# LINE 3580 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 3585 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 3590 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 3595 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3607 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabel
                   {-# LINE 3612 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 3617 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 3622 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3632 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 3637 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3652 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3657 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3662 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 3667 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 3672 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3677 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3682 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIedgeListflow,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIflowpp,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodeListflow,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName _eOstruct 
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIdeclarations,_sIedgeList,_sIedgeListflow,_sIexpected,_sIexstractFunctions,_sIexstractParameters,_sIfinal,_sIflow,_sIflowpp,_sIinit,_sIlabel,_sIlabels,_sIlabstruct,_sImapping,_sInodeList,_sInodeListflow,_sInodes,_sIparamMapping,_sIpp,_sIppcfg,_sIremoved,_sIself,_sIsimplified,_sIwarnings) =
                  s_ _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOsimplifiedName _sOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Expect :: T_Node  ->
                   TypeSet ->
                   T_Node 
sem_Node_Expect expr_ ty_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _exprOdeclaration :: Declaration
              _exprOdeclarations' :: (Map String Declaration)
              _exprOmapping :: Mapping
              _exprOsimplifiedName :: (Maybe Node)
              _exprOstruct :: String
              _exprIannotated :: Node 
              _exprIblocks :: (IntMap (Block Node))
              _exprIcallMapping :: (IntMap Node)
              _exprIconstraints :: (Set Constraint)
              _exprIdeclarations :: (Map String Declaration)
              _exprIedgeList :: ([UEdge])
              _exprIedgeListflow :: ([UEdge])
              _exprIexpected :: (Set Constraint)
              _exprIexstractFunctions :: Node 
              _exprIexstractParameters :: Node 
              _exprIfinal :: (Maybe [Label])
              _exprIflow :: Flow
              _exprIflowpp :: Doc
              _exprIinit :: (Maybe Label)
              _exprIlabel :: Label
              _exprIlabels :: Label
              _exprIlabstruct :: (IntMap String)
              _exprImapping :: Mapping
              _exprInodeList :: ([LNode String])
              _exprInodeListflow :: ([LNode String])
              _exprInodes :: (IntMap Node)
              _exprIparamMapping :: (IntMap Node)
              _exprIpp :: Doc
              _exprIppcfg :: Doc
              _exprIremoved :: Node 
              _exprIself :: Node 
              _exprIsimplified :: Node 
              _exprIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 3766 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 3771 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 3776 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 3781 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 3786 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 3791 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 3796 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 77 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :<=: _exprIlabel) `S.union` _exprIconstraints
                   {-# LINE 3801 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.singleton (_exprIlabel :==: ty_) `S.union` _exprIexpected
                   {-# LINE 3806 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 47 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _exprInodes
                   {-# LINE 3811 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 3816 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 46 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, "expect: " ++ render _exprIpp ++ " == " ++ show ty_)]
                   {-# LINE 3821 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 109 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "## Expect: " >|< _exprIpp >|< text " == " >|< text (show ty_)
                   {-# LINE 3826 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup7 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_exprOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 3833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 3838 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIblocks
                   {-# LINE 3843 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exprIcallMapping
                   {-# LINE 3848 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 3853 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIdeclarations
                   {-# LINE 3858 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _exprIedgeList
                   {-# LINE 3863 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIedgeListflow
                   {-# LINE 3868 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 3873 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIflow
                   {-# LINE 3878 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _exprIflowpp
                   {-# LINE 3883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _exprIlabstruct
                   {-# LINE 3888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprInodeListflow
                   {-# LINE 3893 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 3898 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exprIparamMapping
                   {-# LINE 3903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _exprIppcfg
                   {-# LINE 3908 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIannotated ty_
                   {-# LINE 3913 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIexstractFunctions ty_
                   {-# LINE 3918 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIexstractParameters ty_
                   {-# LINE 3923 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIremoved ty_
                   {-# LINE 3928 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Expect _exprIself ty_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIsimplified ty_
                   {-# LINE 3935 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 3940 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 3945 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 3950 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3955 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3962 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIlabels
                   {-# LINE 3967 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _exprImapping
                   {-# LINE 3972 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3977 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3982 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3987 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3992 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3997 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _exprIannotated,_exprIblocks,_exprIcallMapping,_exprIconstraints,_exprIdeclarations,_exprIedgeList,_exprIedgeListflow,_exprIexpected,_exprIexstractFunctions,_exprIexstractParameters,_exprIfinal,_exprIflow,_exprIflowpp,_exprIinit,_exprIlabel,_exprIlabels,_exprIlabstruct,_exprImapping,_exprInodeList,_exprInodeListflow,_exprInodes,_exprIparamMapping,_exprIpp,_exprIppcfg,_exprIremoved,_exprIself,_exprIsimplified,_exprIwarnings) =
                  expr_ _exprOdeclaration _exprOdeclarations' _exprOlabels _exprOmapping _exprOsimplifiedName _exprOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Expr :: T_Node  ->
                 T_Node 
sem_Node_Expr e_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOedgeList :: ([UEdge])
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOremoved :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOwarnings :: (Set Warning)
              _lhsOppcfg :: Doc
              _eOstruct :: String
              __tup8 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOannotated :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOsimplifiedName :: (Maybe Node)
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIedgeListflow :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIflowpp :: Doc
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodeListflow :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 4078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 4083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 4088 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _eIlabel, ())]
                   {-# LINE 4093 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 4098 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "expr")]
                   {-# LINE 4103 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4108 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4113 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4118 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 4123 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 4128 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   case _eIself of
                       (Assign rv (FunctionCall (FunctionName name) params)) -> SimplifiedFunctionCall name params $ Just rv
                       (FunctionCall (FunctionName name) params)             -> SimplifiedFunctionCall name params Nothing
                       copy                                                  -> Expr copy
                   {-# LINE 4136 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   exstractFunctions (Expr _eIexstractFunctions) _eIcallMapping
                   {-# LINE 4141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 4146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 4151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eInodes
                   {-# LINE 4156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 4161 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text ("struct" ++ show _label) >|< text " [label=\"" >|<
                   _eIppcfg >|< text "\"];"
                   {-# LINE 4167 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "struct" ++ show _label
                   {-# LINE 4172 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup8 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup8
                   {-# LINE 4179 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup8
                   {-# LINE 4184 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 4189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 4194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 4204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 4209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIedgeListflow
                   {-# LINE 4214 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 4219 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 4224 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIflowpp
                   {-# LINE 4229 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 4234 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 4239 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodeListflow
                   {-# LINE 4244 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 4249 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 4254 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp
                   {-# LINE 4259 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIannotated
                   {-# LINE 4264 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIexstractFunctions
                   {-# LINE 4269 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIexstractParameters
                   {-# LINE 4274 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIremoved
                   {-# LINE 4279 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Expr _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIsimplified
                   {-# LINE 4286 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 4291 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 4296 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4303 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 4308 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 4313 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4318 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4323 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4328 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4333 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIedgeListflow,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIflowpp,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodeListflow,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName _eOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_FunctionCall :: T_Node  ->
                         T_ParamList  ->
                         T_Node 
sem_Node_FunctionCall name_ params_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOannotated :: Node 
              _lhsOcallMapping :: (IntMap Node)
              _lhsOexstractFunctions :: Node 
              _lhsOpp :: Doc
              __tup9 :: ((Label,Label))
              _nameOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _nameOdeclaration :: Declaration
              _nameOdeclarations' :: (Map String Declaration)
              _nameOmapping :: Mapping
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
              _nameIedgeListflow :: ([UEdge])
              _nameIexpected :: (Set Constraint)
              _nameIexstractFunctions :: Node 
              _nameIexstractParameters :: Node 
              _nameIfinal :: (Maybe [Label])
              _nameIflow :: Flow
              _nameIflowpp :: Doc
              _nameIinit :: (Maybe Label)
              _nameIlabel :: Label
              _nameIlabels :: Label
              _nameIlabstruct :: (IntMap String)
              _nameImapping :: Mapping
              _nameInodeList :: ([LNode String])
              _nameInodeListflow :: ([LNode String])
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
              _paramsIexstractFunctions :: ParamList 
              _paramsIexstractParameters :: ParamList 
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
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4434 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4439 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4444 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   case _nameIself of
                       (FunctionName "check")   -> buildExpect _paramsIself
                       otherwise                -> _self
                   {-# LINE 4451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 82 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.singleton _label _self
                   {-# LINE 4456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   buildVariable _label
                   {-# LINE 4461 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _nameIpp >|< text "()"
                   {-# LINE 4466 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup9 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_nameOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup9
                   {-# LINE 4473 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup9
                   {-# LINE 4478 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIblocks
                   {-# LINE 4483 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nameIconstraints
                   {-# LINE 4488 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIdeclarations `M.union` _paramsIdeclarations
                   {-# LINE 4493 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nameIedgeList
                   {-# LINE 4498 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIedgeListflow
                   {-# LINE 4503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nameIexpected
                   {-# LINE 4508 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIfinal
                   {-# LINE 4513 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIflow
                   {-# LINE 4518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nameIflowpp
                   {-# LINE 4523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIinit
                   {-# LINE 4528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nameIlabstruct
                   {-# LINE 4533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nameInodeList
                   {-# LINE 4538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameInodeListflow
                   {-# LINE 4543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameInodes `IM.union` _paramsInodes
                   {-# LINE 4548 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nameIparamMapping `IM.union` _paramsIparamMapping
                   {-# LINE 4553 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nameIppcfg
                   {-# LINE 4558 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nameIwarnings
                   {-# LINE 4563 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIannotated _paramsIannotated
                   {-# LINE 4568 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIexstractFunctions _paramsIexstractFunctions
                   {-# LINE 4573 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIexstractParameters _paramsIexstractParameters
                   {-# LINE 4578 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIremoved _paramsIremoved
                   {-# LINE 4583 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionCall _nameIself _paramsIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIsimplified _paramsIsimplified
                   {-# LINE 4590 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 4595 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 4600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4607 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 4612 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 4617 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4622 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4632 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4637 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 4642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4652 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIlabels
                   {-# LINE 4657 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nameImapping
                   {-# LINE 4662 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4667 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _nameIannotated,_nameIblocks,_nameIcallMapping,_nameIconstraints,_nameIdeclarations,_nameIedgeList,_nameIedgeListflow,_nameIexpected,_nameIexstractFunctions,_nameIexstractParameters,_nameIfinal,_nameIflow,_nameIflowpp,_nameIinit,_nameIlabel,_nameIlabels,_nameIlabstruct,_nameImapping,_nameInodeList,_nameInodeListflow,_nameInodes,_nameIparamMapping,_nameIpp,_nameIppcfg,_nameIremoved,_nameIself,_nameIsimplified,_nameIwarnings) =
                  name_ _nameOdeclaration _nameOdeclarations' _nameOlabels _nameOmapping _nameOsimplifiedName _nameOstruct 
              ( _paramsIannotated,_paramsIcallMapping,_paramsIdeclarations,_paramsIexstractFunctions,_paramsIexstractParameters,_paramsIlabel,_paramsIlabels,_paramsImapping,_paramsInodes,_paramsIparamMapping,_paramsIpp,_paramsIremoved,_paramsIself,_paramsIsimplified) =
                  params_ _paramsOdeclaration _paramsOdeclarations' _paramsOlabels _paramsOmapping _paramsOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_FunctionDecl :: String ->
                         T_ParamList  ->
                         T_Node  ->
                         T_Node 
sem_Node_FunctionDecl name_ params_ stmt_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOconstraints :: (Set Constraint)
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflowpp :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _paramsOdeclaration :: Declaration
              _paramsOdeclarations' :: (Map String Declaration)
              _paramsOmapping :: Mapping
              _paramsOsimplifiedName :: (Maybe Node)
              _stmtOdeclarations' :: (Map String Declaration)
              _stmtOlabels :: Label
              _stmtOmapping :: Mapping
              _stmtOsimplifiedName :: (Maybe Node)
              _stmtOstruct :: String
              _paramsIannotated :: ParamList 
              _paramsIcallMapping :: (IntMap Node)
              _paramsIdeclarations :: (Map String Declaration)
              _paramsIexstractFunctions :: ParamList 
              _paramsIexstractParameters :: ParamList 
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
              _stmtIedgeListflow :: ([UEdge])
              _stmtIexpected :: (Set Constraint)
              _stmtIexstractFunctions :: Node 
              _stmtIexstractParameters :: Node 
              _stmtIfinal :: (Maybe [Label])
              _stmtIflow :: Flow
              _stmtIflowpp :: Doc
              _stmtIinit :: (Maybe Label)
              _stmtIlabel :: Label
              _stmtIlabels :: Label
              _stmtIlabstruct :: (IntMap String)
              _stmtImapping :: Mapping
              _stmtInodeList :: ([LNode String])
              _stmtInodeListflow :: ([LNode String])
              _stmtInodes :: (IntMap Node)
              _stmtIparamMapping :: (IntMap Node)
              _stmtIpp :: Doc
              _stmtIppcfg :: Doc
              _stmtIremoved :: Node 
              _stmtIself :: Node 
              _stmtIsimplified :: Node 
              _stmtIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 189 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 4772 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 189 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.fromList [(_ln, Entry _self)
                                  ,(_lx, Exit _self)]
                   {-# LINE 4778 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 171 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _declarations_augmented_syn [_declarations_augmented_f1]
                   {-# LINE 4783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declarations_augmented_f1 =
                  ({-# LINE 171 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.union $ M.singleton name_ _declaration
                   {-# LINE 4788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 4793 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_ln, _stmtIlabel, ())]
                   {-# LINE 4798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 137 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4803 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 137 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_ln, fromJust _stmtIinit)]
                   {-# LINE 4808 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 4813 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_ln, "function " ++ name_)]
                   {-# LINE 4818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4823 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 75 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 4833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 113 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 4838 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declaration =
                  ({-# LINE 169 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Declaration name_ _ln _lx
                   {-# LINE 4843 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclaration =
                  ({-# LINE 170 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _declaration
                   {-# LINE 4848 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 4853 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _label =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _ln
                   {-# LINE 4858 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 102 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "function " >|< text name_ >|< text "() {" >-< indent 4 _stmtIpp >-< text "}"
                   {-# LINE 4863 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup10 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, ln) -> case nextUnique __cont of { (__cont, lx) -> (__cont, ln,lx)}} )
              (_paramsOlabels,_,_) =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 4870 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_ln,_) =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 4875 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_lx) =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 4880 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 189 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIblocks
                   {-# LINE 4885 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIcallMapping `IM.union` _stmtIcallMapping
                   {-# LINE 4890 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtIconstraints
                   {-# LINE 4895 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declarations_augmented_syn =
                  ({-# LINE 171 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIdeclarations `M.union` _stmtIdeclarations
                   {-# LINE 4900 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _stmtIedgeList
                   {-# LINE 4905 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIedgeListflow
                   {-# LINE 4910 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _stmtIexpected
                   {-# LINE 4915 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 137 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIflow
                   {-# LINE 4920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _stmtIflowpp
                   {-# LINE 4925 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _stmtIlabstruct
                   {-# LINE 4930 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _stmtInodeList
                   {-# LINE 4935 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtInodeListflow
                   {-# LINE 4940 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsInodes `IM.union` _stmtInodes
                   {-# LINE 4945 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIparamMapping `IM.union` _stmtIparamMapping
                   {-# LINE 4950 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _stmtIppcfg
                   {-# LINE 4955 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIannotated _stmtIannotated
                   {-# LINE 4960 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIexstractFunctions _stmtIexstractFunctions
                   {-# LINE 4965 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIexstractParameters _stmtIexstractParameters
                   {-# LINE 4970 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIremoved _stmtIremoved
                   {-# LINE 4975 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionDecl name_ _paramsIself _stmtIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIsimplified _stmtIsimplified
                   {-# LINE 4982 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 4987 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 4992 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 4997 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5002 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5009 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5014 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIlabels
                   {-# LINE 5019 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtImapping
                   {-# LINE 5024 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _declaration
                   {-# LINE 5029 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5034 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5039 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5044 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5049 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 5054 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 5059 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5069 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _paramsIannotated,_paramsIcallMapping,_paramsIdeclarations,_paramsIexstractFunctions,_paramsIexstractParameters,_paramsIlabel,_paramsIlabels,_paramsImapping,_paramsInodes,_paramsIparamMapping,_paramsIpp,_paramsIremoved,_paramsIself,_paramsIsimplified) =
                  params_ _paramsOdeclaration _paramsOdeclarations' _paramsOlabels _paramsOmapping _paramsOsimplifiedName 
              ( _stmtIannotated,_stmtIblocks,_stmtIcallMapping,_stmtIconstraints,_stmtIdeclarations,_stmtIedgeList,_stmtIedgeListflow,_stmtIexpected,_stmtIexstractFunctions,_stmtIexstractParameters,_stmtIfinal,_stmtIflow,_stmtIflowpp,_stmtIinit,_stmtIlabel,_stmtIlabels,_stmtIlabstruct,_stmtImapping,_stmtInodeList,_stmtInodeListflow,_stmtInodes,_stmtIparamMapping,_stmtIpp,_stmtIppcfg,_stmtIremoved,_stmtIself,_stmtIsimplified,_stmtIwarnings) =
                  stmt_ _stmtOdeclaration _stmtOdeclarations' _stmtOlabels _stmtOmapping _stmtOsimplifiedName _stmtOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_FunctionName :: String ->
                         T_Node 
sem_Node_FunctionName value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5118 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5123 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5128 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text value_
                   {-# LINE 5133 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup11 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup11
                   {-# LINE 5140 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup11
                   {-# LINE 5145 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 5150 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 5155 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 5160 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 5165 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 5170 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 5175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 5190 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 5195 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5200 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 5205 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 5210 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 5215 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 5220 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 5225 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 5230 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5235 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5240 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5245 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5250 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5255 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionName value_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5262 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 5267 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 5272 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 5277 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5282 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5289 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5294 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_GreaterEqual :: T_Node  ->
                         T_Node  ->
                         T_Node 
sem_Node_GreaterEqual l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIedgeListflow :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIflowpp :: Doc
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodeListflow :: ([LNode String])
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
              _rIedgeListflow :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIflowpp :: Doc
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodeListflow :: ([LNode String])
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
                   {-# LINE 5408 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 5413 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 5418 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, ">=")]
                   {-# LINE 5423 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5428 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5433 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5438 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 75 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " >= " >|< _rIpp
                   {-# LINE 5443 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup12 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup12
                   {-# LINE 5450 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup12
                   {-# LINE 5455 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 5460 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 5465 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 5470 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 5475 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 5480 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIedgeListflow ++ _rIedgeListflow
                   {-# LINE 5485 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 5490 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 5495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 5500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIflowpp >|< _rIflowpp
                   {-# LINE 5505 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 5510 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 5515 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 5520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodeListflow ++ _rInodeListflow
                   {-# LINE 5525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 5530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 5535 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 5540 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 5545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIannotated _rIannotated
                   {-# LINE 5550 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 5555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIexstractParameters _rIexstractParameters
                   {-# LINE 5560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIremoved _rIremoved
                   {-# LINE 5565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  GreaterEqual _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIsimplified _rIsimplified
                   {-# LINE 5572 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 5577 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 5582 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 5587 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5592 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5599 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 5604 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 5609 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5614 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5619 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5624 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5629 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5634 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5639 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5644 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 5649 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 5654 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5659 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5664 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIedgeListflow,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIflowpp,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodeListflow,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIedgeListflow,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIflowpp,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodeListflow,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_If :: T_Node  ->
               T_Node  ->
               ([Node]) ->
               T_Node  ->
               T_Node 
sem_Node_If c_ l_ elseIfs_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOexpected :: (Set Constraint)
              _lhsOwarnings :: (Set Warning)
              _lhsOpp :: Doc
              __tup13 :: ((Label,Label))
              _cOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeListflow :: ([UEdge])
              _lhsOflowpp :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _cOdeclaration :: Declaration
              _cOdeclarations' :: (Map String Declaration)
              _cOmapping :: Mapping
              _cOsimplifiedName :: (Maybe Node)
              _cOstruct :: String
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOlabels :: Label
              _lOmapping :: Mapping
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _cIannotated :: Node 
              _cIblocks :: (IntMap (Block Node))
              _cIcallMapping :: (IntMap Node)
              _cIconstraints :: (Set Constraint)
              _cIdeclarations :: (Map String Declaration)
              _cIedgeList :: ([UEdge])
              _cIedgeListflow :: ([UEdge])
              _cIexpected :: (Set Constraint)
              _cIexstractFunctions :: Node 
              _cIexstractParameters :: Node 
              _cIfinal :: (Maybe [Label])
              _cIflow :: Flow
              _cIflowpp :: Doc
              _cIinit :: (Maybe Label)
              _cIlabel :: Label
              _cIlabels :: Label
              _cIlabstruct :: (IntMap String)
              _cImapping :: Mapping
              _cInodeList :: ([LNode String])
              _cInodeListflow :: ([LNode String])
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
              _lIedgeListflow :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIflowpp :: Doc
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodeListflow :: ([LNode String])
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
              _rIedgeListflow :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIflowpp :: Doc
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodeListflow :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 5818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 5823 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 5828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _cIlabel, ()), (_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 5833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 131 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5838 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 131 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, fromJust _lIinit), (_label, fromJust _rIinit)]
                   {-# LINE 5843 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 5848 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "if")]
                   {-# LINE 5853 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5858 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5863 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5868 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 5873 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 109 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   pure (++) <*> _lIfinal <*> _rIfinal
                   {-# LINE 5878 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cIconstraints
                   {-# LINE 5883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.fromList [(_cIlabel :==: S.singleton TyBool)] `S.union` _cIexpected
                   {-# LINE 5888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 5893 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _cInodes
                   {-# LINE 5898 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 5903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "if (" >|< _cIpp >|< text ") {" >-< indent 4 _lIpp >-< text "} else {" >-< indent 4 _rIpp >-< text "}"
                   {-# LINE 5908 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup13 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_cOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup13
                   {-# LINE 5915 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup13
                   {-# LINE 5920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIblocks `IM.union` _lIblocks `IM.union` _rIblocks
                   {-# LINE 5925 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIcallMapping `IM.union` _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 5930 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 5935 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIdeclarations `M.union` _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 5940 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cIedgeList ++ _lIedgeList ++ _rIedgeList
                   {-# LINE 5945 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIedgeListflow ++ _lIedgeListflow ++ _rIedgeListflow
                   {-# LINE 5950 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 131 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIflow ++ _lIflow ++ _rIflow
                   {-# LINE 5955 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIflowpp >|< _lIflowpp >|< _rIflowpp
                   {-# LINE 5960 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIlabstruct `IM.union` _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 5965 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cInodeList ++ _lInodeList ++ _rInodeList
                   {-# LINE 5970 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cInodeListflow ++ _lInodeListflow ++ _rInodeListflow
                   {-# LINE 5975 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 5980 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIparamMapping `IM.union` _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 5985 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIppcfg >|< _lIppcfg >|< _rIppcfg
                   {-# LINE 5990 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIannotated _lIannotated elseIfs_ _rIannotated
                   {-# LINE 5995 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIexstractFunctions _lIexstractFunctions elseIfs_ _rIexstractFunctions
                   {-# LINE 6000 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIexstractParameters _lIexstractParameters elseIfs_ _rIexstractParameters
                   {-# LINE 6005 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIremoved _lIremoved elseIfs_ _rIremoved
                   {-# LINE 6010 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  If _cIself _lIself elseIfs_ _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIsimplified _lIsimplified elseIfs_ _rIsimplified
                   {-# LINE 6017 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 6022 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 6027 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 6032 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6037 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6044 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 6049 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 6054 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6059 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6069 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6074 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6079 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6089 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIlabels
                   {-# LINE 6094 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cImapping
                   {-# LINE 6099 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6104 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6109 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6114 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6119 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 6124 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 6129 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6134 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6139 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _cIannotated,_cIblocks,_cIcallMapping,_cIconstraints,_cIdeclarations,_cIedgeList,_cIedgeListflow,_cIexpected,_cIexstractFunctions,_cIexstractParameters,_cIfinal,_cIflow,_cIflowpp,_cIinit,_cIlabel,_cIlabels,_cIlabstruct,_cImapping,_cInodeList,_cInodeListflow,_cInodes,_cIparamMapping,_cIpp,_cIppcfg,_cIremoved,_cIself,_cIsimplified,_cIwarnings) =
                  c_ _cOdeclaration _cOdeclarations' _cOlabels _cOmapping _cOsimplifiedName _cOstruct 
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIedgeListflow,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIflowpp,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodeListflow,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIedgeListflow,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIflowpp,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodeListflow,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_IsEqual :: T_Node  ->
                    T_Node  ->
                    T_Node 
sem_Node_IsEqual l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOedgeListflow :: ([UEdge])
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIedgeListflow :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIflowpp :: Doc
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodeListflow :: ([LNode String])
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
              _rIedgeListflow :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIflowpp :: Doc
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodeListflow :: ([LNode String])
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
                   {-# LINE 6259 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6264 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 6269 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 6274 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 6279 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.singleton (_lIlabel :<=: _rIlabel)
                   {-# LINE 6284 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6289 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "==")]
                   {-# LINE 6294 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6299 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6304 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6309 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " == " >|< _rIpp
                   {-# LINE 6314 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup14 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup14
                   {-# LINE 6321 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup14
                   {-# LINE 6326 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 6331 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 6336 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 6341 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 6346 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 6351 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIedgeListflow ++ _rIedgeListflow
                   {-# LINE 6356 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 6361 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 6366 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 6371 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIflowpp >|< _rIflowpp
                   {-# LINE 6376 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 6381 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 6386 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 6391 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodeListflow ++ _rInodeListflow
                   {-# LINE 6396 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 6401 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 6406 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 6411 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 6416 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIannotated _rIannotated
                   {-# LINE 6421 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 6426 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIexstractParameters _rIexstractParameters
                   {-# LINE 6431 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIremoved _rIremoved
                   {-# LINE 6436 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  IsEqual _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIsimplified _rIsimplified
                   {-# LINE 6443 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 6448 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 6453 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 6458 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6463 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6470 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 6475 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 6480 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6485 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6490 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6505 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6510 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6515 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 6520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 6525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6535 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIedgeListflow,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIflowpp,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodeListflow,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIedgeListflow,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIflowpp,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodeListflow,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_LFalse :: T_Node 
sem_Node_LFalse  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOconstraints :: (Set Constraint)
              _lhsOpp :: Doc
              __tup15 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOnodeList =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6583 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "false")]
                   {-# LINE 6588 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6593 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6598 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6603 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6608 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "false"
                   {-# LINE 6613 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup15 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup15
                   {-# LINE 6620 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup15
                   {-# LINE 6625 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6630 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6635 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 6640 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6645 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6650 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6655 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 6660 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6665 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 6670 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 6675 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 6680 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6685 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6690 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6695 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6700 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 6705 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6710 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6715 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6720 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6725 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6730 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  LFalse
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6737 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 6742 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 6747 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 6752 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6757 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6764 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6769 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_LTrue :: T_Node 
sem_Node_LTrue  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOconstraints :: (Set Constraint)
              _lhsOpp :: Doc
              __tup16 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOnodeList =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6813 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "true")]
                   {-# LINE 6818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6823 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6838 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "true"
                   {-# LINE 6843 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup16 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup16
                   {-# LINE 6850 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup16
                   {-# LINE 6855 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6860 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6865 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 6870 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6875 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6880 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6885 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 6890 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6895 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 6900 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 6905 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 6910 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6915 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6925 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6930 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 6935 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6940 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6945 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6950 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6955 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6960 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  LTrue
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6967 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 6972 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 6977 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 6982 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6987 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6994 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6999 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Literal :: String ->
                    T_Node 
sem_Node_Literal value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup17 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7044 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7049 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7054 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup17 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup17
                   {-# LINE 7061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup17
                   {-# LINE 7066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 7081 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 7086 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7091 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 7096 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7101 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 7106 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 7111 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 7116 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 7121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 7126 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7131 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 7136 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 7151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 7156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7161 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7166 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7171 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7176 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7181 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Literal value_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7188 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 7193 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 7198 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 7203 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7208 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7215 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7220 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Min :: T_Node  ->
                T_Node  ->
                T_Node 
sem_Node_Min l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOedgeListflow :: ([UEdge])
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIedgeListflow :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIflowpp :: Doc
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodeListflow :: ([LNode String])
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
              _rIedgeListflow :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIflowpp :: Doc
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodeListflow :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 7334 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 7339 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 7344 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 7349 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7354 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 7359 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7364 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "-")]
                   {-# LINE 7369 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7374 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7379 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7384 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " - " >|< _rIpp
                   {-# LINE 7389 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup18 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup18
                   {-# LINE 7396 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup18
                   {-# LINE 7401 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 7406 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 7411 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 7416 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 7421 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 7426 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIedgeListflow ++ _rIedgeListflow
                   {-# LINE 7431 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 7436 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 7441 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 7446 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIflowpp >|< _rIflowpp
                   {-# LINE 7451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 7456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 7461 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 7466 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodeListflow ++ _rInodeListflow
                   {-# LINE 7471 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 7476 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 7481 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 7486 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 7491 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIannotated _rIannotated
                   {-# LINE 7496 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 7501 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIexstractParameters _rIexstractParameters
                   {-# LINE 7506 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIremoved _rIremoved
                   {-# LINE 7511 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Min _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIsimplified _rIsimplified
                   {-# LINE 7518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 7523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 7528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 7533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 7550 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 7555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7570 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7575 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7580 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7585 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7590 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 7595 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 7600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7605 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7610 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIedgeListflow,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIflowpp,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodeListflow,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIedgeListflow,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIflowpp,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodeListflow,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Mod :: T_Node  ->
                T_Node  ->
                T_Node 
sem_Node_Mod l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOedgeListflow :: ([UEdge])
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIedgeListflow :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIflowpp :: Doc
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodeListflow :: ([LNode String])
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
              _rIedgeListflow :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIflowpp :: Doc
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodeListflow :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 7728 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 7733 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 7738 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 7743 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7748 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: S.singleton TyInt), (_rIlabel :==: S.singleton TyInt)]
                   {-# LINE 7753 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7758 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "%")]
                   {-# LINE 7763 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7768 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7773 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7778 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 70 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " % " >|< _rIpp
                   {-# LINE 7783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup19 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup19
                   {-# LINE 7790 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup19
                   {-# LINE 7795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 7800 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 7805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 7810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 7815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 7820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIedgeListflow ++ _rIedgeListflow
                   {-# LINE 7825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 7830 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 7835 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 7840 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIflowpp >|< _rIflowpp
                   {-# LINE 7845 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 7850 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 7855 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 7860 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodeListflow ++ _rInodeListflow
                   {-# LINE 7865 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 7870 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 7875 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 7880 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 7885 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIannotated _rIannotated
                   {-# LINE 7890 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 7895 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIexstractParameters _rIexstractParameters
                   {-# LINE 7900 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIremoved _rIremoved
                   {-# LINE 7905 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Mod _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIsimplified _rIsimplified
                   {-# LINE 7912 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 7917 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 7922 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 7927 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7932 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7939 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 7944 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 7949 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7954 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7959 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7964 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7969 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7974 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7979 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7984 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 7989 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 7994 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7999 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8004 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIedgeListflow,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIflowpp,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodeListflow,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIedgeListflow,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIflowpp,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodeListflow,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Mul :: T_Node  ->
                T_Node  ->
                T_Node 
sem_Node_Mul l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOedgeListflow :: ([UEdge])
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIedgeListflow :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIflowpp :: Doc
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodeListflow :: ([LNode String])
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
              _rIedgeListflow :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIflowpp :: Doc
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodeListflow :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 8122 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 8127 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 8132 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 8137 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 8142 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 8147 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 8152 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "*")]
                   {-# LINE 8157 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8162 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8167 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8172 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 68 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " * " >|< _rIpp
                   {-# LINE 8177 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup20 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup20
                   {-# LINE 8184 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup20
                   {-# LINE 8189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 8194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 8199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 8204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 8209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 8214 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIedgeListflow ++ _rIedgeListflow
                   {-# LINE 8219 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 8224 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 8229 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 8234 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIflowpp >|< _rIflowpp
                   {-# LINE 8239 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 8244 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 8249 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 8254 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodeListflow ++ _rInodeListflow
                   {-# LINE 8259 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 8264 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 8269 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 8274 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 8279 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIannotated _rIannotated
                   {-# LINE 8284 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 8289 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIexstractParameters _rIexstractParameters
                   {-# LINE 8294 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIremoved _rIremoved
                   {-# LINE 8299 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Mul _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIsimplified _rIsimplified
                   {-# LINE 8306 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 8311 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 8316 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 8321 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8326 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8333 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 8338 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 8343 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8348 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8353 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8358 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8363 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8368 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8373 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8378 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 8383 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 8388 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8393 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8398 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIedgeListflow,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIflowpp,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodeListflow,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIedgeListflow,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIflowpp,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodeListflow,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_OpenTag :: T_Node 
sem_Node_OpenTag  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup21 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8446 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup21 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 8463 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 8468 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 8473 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 8478 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 8483 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 8488 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 8493 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 8498 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 8503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 8508 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 8513 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 8518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 8523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 8528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 8533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 8538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 8543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 8548 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 8553 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 8558 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 8563 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8568 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8573 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8578 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8583 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  OpenTag
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8590 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 8595 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 8600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 8605 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8610 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8617 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8622 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Or :: T_Node  ->
               T_Node  ->
               T_Node 
sem_Node_Or l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOedgeListflow :: ([UEdge])
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIedgeListflow :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIflowpp :: Doc
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodeListflow :: ([LNode String])
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
              _rIedgeListflow :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIflowpp :: Doc
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodeListflow :: ([LNode String])
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
                   {-# LINE 8736 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 8741 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 8746 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 8751 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 8756 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: S.singleton TyBool), (_rIlabel :==: S.singleton TyBool)]
                   {-# LINE 8761 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 8766 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "||")]
                   {-# LINE 8771 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8776 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8781 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8786 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 82 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " || " >|< _rIpp
                   {-# LINE 8791 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup22 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup22
                   {-# LINE 8798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup22
                   {-# LINE 8803 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 8808 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 8813 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 8818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 8823 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 8828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIedgeListflow ++ _rIedgeListflow
                   {-# LINE 8833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 8838 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 8843 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 8848 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIflowpp >|< _rIflowpp
                   {-# LINE 8853 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 8858 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 8863 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 8868 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodeListflow ++ _rInodeListflow
                   {-# LINE 8873 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 8878 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 8883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 8888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 8893 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIannotated _rIannotated
                   {-# LINE 8898 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 8903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIexstractParameters _rIexstractParameters
                   {-# LINE 8908 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIremoved _rIremoved
                   {-# LINE 8913 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Or _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIsimplified _rIsimplified
                   {-# LINE 8920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 8925 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 8930 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 8935 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8940 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8947 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 8952 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 8957 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8962 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8967 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8972 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8977 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8982 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8987 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8992 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 8997 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 9002 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9007 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9012 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIedgeListflow,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIflowpp,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodeListflow,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIedgeListflow,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIflowpp,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodeListflow,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Param :: T_Node  ->
                  T_Node 
sem_Node_Param e_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOsimplifiedName :: (Maybe Node)
              _eOstruct :: String
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIedgeListflow :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIflowpp :: Doc
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodeListflow :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9095 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 112 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.singleton _label _eIself
                   {-# LINE 9110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 104 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp
                   {-# LINE 9115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup23 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup23
                   {-# LINE 9122 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup23
                   {-# LINE 9127 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 9132 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 9137 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 9142 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 9147 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 9152 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIedgeListflow
                   {-# LINE 9157 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 9162 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 9167 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 9172 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIflowpp
                   {-# LINE 9177 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 9182 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 9187 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 9192 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodeListflow
                   {-# LINE 9197 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 9202 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg
                   {-# LINE 9207 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings
                   {-# LINE 9212 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIannotated
                   {-# LINE 9217 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIexstractFunctions
                   {-# LINE 9222 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIexstractParameters
                   {-# LINE 9227 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIremoved
                   {-# LINE 9232 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Param _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIsimplified
                   {-# LINE 9239 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 9244 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 9249 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 9254 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9259 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9266 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 9271 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 9276 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9281 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9286 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9291 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9296 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9301 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIedgeListflow,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIflowpp,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodeListflow,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName _eOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Plus :: T_Node  ->
                 T_Node  ->
                 T_Node 
sem_Node_Plus l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOedgeListflow :: ([UEdge])
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIedgeListflow :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIflowpp :: Doc
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodeListflow :: ([LNode String])
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
              _rIedgeListflow :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIflowpp :: Doc
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodeListflow :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 9417 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 9422 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 9427 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 9432 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 9437 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 9442 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 9447 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "+")]
                   {-# LINE 9452 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9457 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9462 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9467 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " + " >|< _rIpp
                   {-# LINE 9472 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup24 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup24
                   {-# LINE 9479 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup24
                   {-# LINE 9484 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 9489 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 9494 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 9499 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 9504 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 9509 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIedgeListflow ++ _rIedgeListflow
                   {-# LINE 9514 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 9519 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 9524 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 9529 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIflowpp >|< _rIflowpp
                   {-# LINE 9534 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 9539 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 9544 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 9549 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodeListflow ++ _rInodeListflow
                   {-# LINE 9554 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 9559 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 9564 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 9569 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 9574 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIannotated _rIannotated
                   {-# LINE 9579 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 9584 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIexstractParameters _rIexstractParameters
                   {-# LINE 9589 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIremoved _rIremoved
                   {-# LINE 9594 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Plus _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIsimplified _rIsimplified
                   {-# LINE 9601 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 9606 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 9611 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 9616 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9621 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9628 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 9633 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 9638 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9643 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9648 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9653 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9658 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9663 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9668 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9673 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 9678 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 9683 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9688 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9693 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIedgeListflow,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIflowpp,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodeListflow,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIedgeListflow,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIflowpp,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodeListflow,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Print :: T_Node  ->
                  T_Node 
sem_Node_Print e_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOsimplifiedName :: (Maybe Node)
              _eOstruct :: String
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIedgeListflow :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIflowpp :: Doc
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodeListflow :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9776 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9781 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9786 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 117 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "print " >|< _eIpp
                   {-# LINE 9791 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup25 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup25
                   {-# LINE 9798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup25
                   {-# LINE 9803 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 9808 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 9813 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 9818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 9823 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 9828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIedgeListflow
                   {-# LINE 9833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 9838 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 9843 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 9848 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIflowpp
                   {-# LINE 9853 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 9858 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 9863 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 9868 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodeListflow
                   {-# LINE 9873 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 9878 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 9883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg
                   {-# LINE 9888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings
                   {-# LINE 9893 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIannotated
                   {-# LINE 9898 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIexstractFunctions
                   {-# LINE 9903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIexstractParameters
                   {-# LINE 9908 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIremoved
                   {-# LINE 9913 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Print _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIsimplified
                   {-# LINE 9920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 9925 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 9930 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 9935 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9940 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9947 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 9952 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 9957 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9962 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9967 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9972 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9977 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9982 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIedgeListflow,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIflowpp,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodeListflow,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName _eOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Return :: T_Node  ->
                   T_Node 
sem_Node_Return e_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOexstractFunctions :: Node 
              _lhsOmapping :: Mapping
              _lhsOwarnings :: (Set Warning)
              _lhsOpp :: Doc
              __tup26 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflowpp :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOannotated :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOsimplifiedName :: (Maybe Node)
              _eOstruct :: String
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIedgeListflow :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIflowpp :: Doc
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodeListflow :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 10063 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 10068 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 10073 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _eIlabel, ())]
                   {-# LINE 10078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 135 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 10083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 135 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, lx _lhsIdeclaration)]
                   {-# LINE 10088 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 10093 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "return")]
                   {-# LINE 10098 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10103 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10108 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10113 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 10118 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 10123 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 93 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   exstractFunctions (Return _eIexstractFunctions) _eIcallMapping
                   {-# LINE 10128 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 10133 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   updateMapping ReturnValue                  _eIlabel 0                 _constraints _eImapping
                   {-# LINE 10138 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 10143 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eInodes
                   {-# LINE 10148 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 10153 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "return " >|< _eIpp
                   {-# LINE 10158 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup26 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup26
                   {-# LINE 10165 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup26
                   {-# LINE 10170 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 10175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 10180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 10185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 10190 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 10195 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIedgeListflow
                   {-# LINE 10200 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 10205 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 135 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 10210 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIflowpp
                   {-# LINE 10215 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 10220 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 10225 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodeListflow
                   {-# LINE 10230 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 10235 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 10240 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg
                   {-# LINE 10245 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIannotated
                   {-# LINE 10250 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIexstractFunctions
                   {-# LINE 10255 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIexstractParameters
                   {-# LINE 10260 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIremoved
                   {-# LINE 10265 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Return _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIsimplified
                   {-# LINE 10272 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 10277 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 10282 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10287 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10294 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 10299 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10304 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10309 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10314 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10319 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10324 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIedgeListflow,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIflowpp,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodeListflow,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName _eOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Sequence :: T_Node  ->
                     T_Node  ->
                     T_Node 
sem_Node_Sequence f_ s_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOlabstruct :: (IntMap String)
              __tup27 :: ((Label,Label))
              _fOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _fOdeclaration :: Declaration
              _fOdeclarations' :: (Map String Declaration)
              _fOmapping :: Mapping
              _fOsimplifiedName :: (Maybe Node)
              _fOstruct :: String
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOsimplifiedName :: (Maybe Node)
              _sOstruct :: String
              _fIannotated :: Node 
              _fIblocks :: (IntMap (Block Node))
              _fIcallMapping :: (IntMap Node)
              _fIconstraints :: (Set Constraint)
              _fIdeclarations :: (Map String Declaration)
              _fIedgeList :: ([UEdge])
              _fIedgeListflow :: ([UEdge])
              _fIexpected :: (Set Constraint)
              _fIexstractFunctions :: Node 
              _fIexstractParameters :: Node 
              _fIfinal :: (Maybe [Label])
              _fIflow :: Flow
              _fIflowpp :: Doc
              _fIinit :: (Maybe Label)
              _fIlabel :: Label
              _fIlabels :: Label
              _fIlabstruct :: (IntMap String)
              _fImapping :: Mapping
              _fInodeList :: ([LNode String])
              _fInodeListflow :: ([LNode String])
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
              _sIedgeListflow :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIexstractFunctions :: Node 
              _sIexstractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIflowpp :: Doc
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sIlabstruct :: (IntMap String)
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodeListflow :: ([LNode String])
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
                   {-# LINE 10440 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _fIlabel, ()), (_label, _sIlabel, ())]
                   {-# LINE 10445 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 10450 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) _flow
                   {-# LINE 10455 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _flowpp_augmented_syn [_flowpp_augmented_f1]
                   {-# LINE 10460 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flowpp_augmented_f1 =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   (>-<) $ above [text (buildLabelRef _lstr     l) >|< text "-> " >|< text (buildLabelRef _lstr     l') >|< text ";" | (l,l') <- _flow    ]
                   {-# LINE 10465 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 10470 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, ";")]
                   {-# LINE 10475 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10480 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10485 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10490 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIinit <|> _sIinit
                   {-# LINE 10495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 107 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIfinal <|> _fIfinal
                   {-# LINE 10500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow =
                  ({-# LINE 129 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   if isNothing _sIinit || isNothing _fIfinal then [] else [(l, fromJust _sIinit) | l <- fromJust _fIfinal]
                   {-# LINE 10505 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _fIpp >|< text ";" >-< _sIpp
                   {-# LINE 10510 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 32 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _fIppcfg >-< _sIppcfg
                   {-# LINE 10515 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lstr =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _fIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 10520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lstr
                   {-# LINE 10525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup27 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_fOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup27
                   {-# LINE 10532 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup27
                   {-# LINE 10537 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIblocks `IM.union` _sIblocks
                   {-# LINE 10542 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _fIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 10547 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _fIconstraints `S.union` _sIconstraints
                   {-# LINE 10552 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIdeclarations `M.union` _sIdeclarations
                   {-# LINE 10557 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _fIedgeList ++ _sIedgeList
                   {-# LINE 10562 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIedgeListflow ++ _sIedgeListflow
                   {-# LINE 10567 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _fIexpected `S.union` _sIexpected
                   {-# LINE 10572 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _flow
                   {-# LINE 10577 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flowpp_augmented_syn =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _fIflowpp >|< _sIflowpp
                   {-# LINE 10582 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _fInodeList ++ _sInodeList
                   {-# LINE 10587 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fInodeListflow ++ _sInodeListflow
                   {-# LINE 10592 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fInodes `IM.union` _sInodes
                   {-# LINE 10597 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _fIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 10602 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _fIwarnings `S.union` _sIwarnings
                   {-# LINE 10607 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIannotated _sIannotated
                   {-# LINE 10612 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIexstractFunctions _sIexstractFunctions
                   {-# LINE 10617 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIexstractParameters _sIexstractParameters
                   {-# LINE 10622 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIremoved _sIremoved
                   {-# LINE 10627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Sequence _fIself _sIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIsimplified _sIsimplified
                   {-# LINE 10634 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 10639 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 10644 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 10649 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10654 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10661 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 10666 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 10671 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10676 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10681 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10686 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10691 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10696 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10701 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIlabels
                   {-# LINE 10711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _fImapping
                   {-# LINE 10716 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10721 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10726 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _fIannotated,_fIblocks,_fIcallMapping,_fIconstraints,_fIdeclarations,_fIedgeList,_fIedgeListflow,_fIexpected,_fIexstractFunctions,_fIexstractParameters,_fIfinal,_fIflow,_fIflowpp,_fIinit,_fIlabel,_fIlabels,_fIlabstruct,_fImapping,_fInodeList,_fInodeListflow,_fInodes,_fIparamMapping,_fIpp,_fIppcfg,_fIremoved,_fIself,_fIsimplified,_fIwarnings) =
                  f_ _fOdeclaration _fOdeclarations' _fOlabels _fOmapping _fOsimplifiedName _fOstruct 
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIdeclarations,_sIedgeList,_sIedgeListflow,_sIexpected,_sIexstractFunctions,_sIexstractParameters,_sIfinal,_sIflow,_sIflowpp,_sIinit,_sIlabel,_sIlabels,_sIlabstruct,_sImapping,_sInodeList,_sInodeListflow,_sInodes,_sIparamMapping,_sIpp,_sIppcfg,_sIremoved,_sIself,_sIsimplified,_sIwarnings) =
                  s_ _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOsimplifiedName _sOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Simple :: String ->
                   T_Node 
sem_Node_Simple value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10775 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10780 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10785 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   case M.lookup (Identifier value_) _lhsImapping of
                       Just c  -> S.singleton (_label :==: fromArrayRepeatedly (levels (fromJust _lhsIsimplifiedName)) c)
                       Nothing -> S.singleton (_label :==: S.empty)
                   {-# LINE 10792 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text value_
                   {-# LINE 10797 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 48 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text value_
                   {-# LINE 10802 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup28 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup28
                   {-# LINE 10809 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup28
                   {-# LINE 10814 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10819 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 10824 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 10829 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 10834 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 10839 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 10844 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 10849 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 10854 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 10859 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 10864 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 10869 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 10874 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 10879 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 10889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 10894 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 10899 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 10904 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 10909 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 10914 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Simple value_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 10921 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 10926 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 10931 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 10936 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10941 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10948 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10953 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_SimplifiedFunctionCall :: String ->
                                   T_ParamList  ->
                                   (Maybe Node) ->
                                   T_Node 
sem_Node_SimplifiedFunctionCall name_ params_ result_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOpp :: Doc
              __tup29 :: ((Label,Label,Label,Label,Label))
              _paramsOlabels :: Label
              _la :: Label
              _lb :: Label
              _lc :: Label
              _lr :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflowpp :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
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
              _paramsIexstractFunctions :: ParamList 
              _paramsIexstractParameters :: ParamList 
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
                  ({-# LINE 184 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 11022 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 184 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.fromList [(_lb, Normal Skip)
                                  ,(_lc, Call _lc _lr _self)
                                  ,(_lr, F.Return _lc _lr _self)
                                  ,(_la, Normal Skip)]
                   {-# LINE 11030 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 139 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 11035 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 139 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   let (Declaration name ln lx) = lookupDeclaration name_ _lhsIdeclarations'
                   in (++) [(_lb, _la), (_lb, _lc), (_lr, _la), (_lc, ln), (lx, _lr)]
                   {-# LINE 11041 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11046 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_lc, name_ ++ "() [lb: " ++ show _lb ++ ", lc: " ++ show _lc ++ ", lr: " ++ show _lr ++ ", la: " ++ show _la ++ "]")]
                   {-# LINE 11051 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11056 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _lb
                   {-# LINE 11066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_la]
                   {-# LINE 11071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   exstractFunctions (SimplifiedFunctionCall name_ _paramsIexstractFunctions result_) _paramsIcallMapping
                   {-# LINE 11076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 119 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   exstractParameters _self _paramsIparamMapping
                   {-# LINE 11081 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _label =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lc
                   {-# LINE 11086 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   case result_ of
                      Just v  -> pp v >|< text " := " >|< text name_ >|< text "(" >|< _paramsIpp >|< text ")"
                      Nothing -> text ":: " >|< text name_ >|< text "(" >|< _paramsIpp >|< text ")"
                   {-# LINE 11093 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup29 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, la) -> case nextUnique __cont of { (__cont, lb) -> case nextUnique __cont of { (__cont, lc) -> case nextUnique __cont of { (__cont, lr) -> (__cont, la,lb,lc,lr)}}}} )
              (_paramsOlabels,_,_,_,_) =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_la,_,_,_) =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_lb,_,_) =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_,_lc,_) =
                  ({-# LINE 40 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_,_,_lr) =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11120 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 184 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11125 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIcallMapping
                   {-# LINE 11130 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 11135 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIdeclarations
                   {-# LINE 11140 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11145 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11150 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11155 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 139 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11160 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 11165 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11170 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsInodes
                   {-# LINE 11185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIparamMapping
                   {-# LINE 11190 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 11195 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11200 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIannotated result_
                   {-# LINE 11205 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIexstractFunctions result_
                   {-# LINE 11210 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIexstractParameters result_
                   {-# LINE 11215 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIremoved result_
                   {-# LINE 11220 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  SimplifiedFunctionCall name_ _paramsIself result_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIsimplified result_
                   {-# LINE 11227 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 11232 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11237 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11244 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11249 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 11254 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 11259 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 11264 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 11269 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11274 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 11279 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _paramsIannotated,_paramsIcallMapping,_paramsIdeclarations,_paramsIexstractFunctions,_paramsIexstractParameters,_paramsIlabel,_paramsIlabels,_paramsImapping,_paramsInodes,_paramsIparamMapping,_paramsIpp,_paramsIremoved,_paramsIself,_paramsIsimplified) =
                  params_ _paramsOdeclaration _paramsOdeclarations' _paramsOlabels _paramsOmapping _paramsOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Skip :: T_Node 
sem_Node_Skip  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOpp :: Doc
              __tup30 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOblocks =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 11325 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 11330 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11335 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "[skip]")]
                   {-# LINE 11340 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11345 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11350 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11355 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 11360 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 11365 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 11370 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup30 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup30
                   {-# LINE 11377 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup30
                   {-# LINE 11382 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11387 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11392 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 11397 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 11402 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11407 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11412 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11417 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11422 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 11427 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11432 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11437 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11442 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11447 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11452 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 11457 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11462 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11467 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11472 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11477 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11482 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Skip
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11489 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 11494 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 11499 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 11504 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11509 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11516 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11521 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_String :: String ->
                   T_Node 
sem_Node_String value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup31 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOnodeList =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11566 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, value_)]
                   {-# LINE 11571 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11576 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11581 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11586 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "\"" >|< text value_ >|< text "\""
                   {-# LINE 11591 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup31 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup31
                   {-# LINE 11598 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup31
                   {-# LINE 11603 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11608 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11613 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 11618 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 11623 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11628 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11633 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11638 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 11643 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11648 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 11653 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 11658 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11663 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11668 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11673 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11678 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11683 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 11688 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11693 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11698 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11703 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11708 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11713 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  String value_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11720 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 11725 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 11730 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 11735 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11740 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11747 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11752 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Variable :: T_Node  ->
                     T_Node 
sem_Node_Variable n_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOconstraints :: (Set Constraint)
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
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOedgeListflow :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOflowpp :: Doc
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _nOdeclaration :: Declaration
              _nOdeclarations' :: (Map String Declaration)
              _nOmapping :: Mapping
              _nOstruct :: String
              _nIannotated :: Node 
              _nIblocks :: (IntMap (Block Node))
              _nIcallMapping :: (IntMap Node)
              _nIconstraints :: (Set Constraint)
              _nIdeclarations :: (Map String Declaration)
              _nIedgeList :: ([UEdge])
              _nIedgeListflow :: ([UEdge])
              _nIexpected :: (Set Constraint)
              _nIexstractFunctions :: Node 
              _nIexstractParameters :: Node 
              _nIfinal :: (Maybe [Label])
              _nIflow :: Flow
              _nIflowpp :: Doc
              _nIinit :: (Maybe Label)
              _nIlabel :: Label
              _nIlabels :: Label
              _nIlabstruct :: (IntMap String)
              _nImapping :: Mapping
              _nInodeList :: ([LNode String])
              _nInodeListflow :: ([LNode String])
              _nInodes :: (IntMap Node)
              _nIparamMapping :: (IntMap Node)
              _nIpp :: Doc
              _nIppcfg :: Doc
              _nIremoved :: Node 
              _nIself :: Node 
              _nIsimplified :: Node 
              _nIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 11831 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :<=: _nIlabel)
                   {-# LINE 11836 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 40 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 11841 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 40 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 11846 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11851 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "$" ++ render _nIpp)]
                   {-# LINE 11856 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11861 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11866 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11871 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 77 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 11876 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 103 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 11881 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 44 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName <|> pure _self
                   {-# LINE 11886 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "$" >|< _nIpp
                   {-# LINE 11891 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text "<" >|< text (show _label) >|< text "> " >|< text "$" >|< _nIppcfg
                   {-# LINE 11896 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup32 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_nOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup32
                   {-# LINE 11903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup32
                   {-# LINE 11908 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 11913 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 11918 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 11923 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 11928 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nIedgeList
                   {-# LINE 11933 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIedgeListflow
                   {-# LINE 11938 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 11943 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 11948 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIflowpp
                   {-# LINE 11953 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 40 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIlabstruct
                   {-# LINE 11958 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nInodeList
                   {-# LINE 11963 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nInodeListflow
                   {-# LINE 11968 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 11973 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 11978 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 11983 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIannotated
                   {-# LINE 11988 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIexstractFunctions
                   {-# LINE 11993 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIexstractParameters
                   {-# LINE 11998 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIremoved
                   {-# LINE 12003 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Variable _nIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIsimplified
                   {-# LINE 12010 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 12015 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 12020 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 12025 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 12030 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 12037 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 12042 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 12047 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12052 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12057 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 12062 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 12067 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _nIannotated,_nIblocks,_nIcallMapping,_nIconstraints,_nIdeclarations,_nIedgeList,_nIedgeListflow,_nIexpected,_nIexstractFunctions,_nIexstractParameters,_nIfinal,_nIflow,_nIflowpp,_nIinit,_nIlabel,_nIlabels,_nIlabstruct,_nImapping,_nInodeList,_nInodeListflow,_nInodes,_nIparamMapping,_nIpp,_nIppcfg,_nIremoved,_nIself,_nIsimplified,_nIwarnings) =
                  n_ _nOdeclaration _nOdeclarations' _nOlabels _nOmapping _nOsimplifiedName _nOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_While :: T_Node  ->
                  T_Node  ->
                  T_Node 
sem_Node_While c_ s_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
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
              _lhsOexstractFunctions :: Node 
              _lhsOexpected :: (Set Constraint)
              _lhsOwarnings :: (Set Warning)
              _lhsOpp :: Doc
              __tup33 :: ((Label,Label))
              _cOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeListflow :: ([UEdge])
              _lhsOflowpp :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeListflow :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOannotated :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _cOdeclaration :: Declaration
              _cOdeclarations' :: (Map String Declaration)
              _cOmapping :: Mapping
              _cOsimplifiedName :: (Maybe Node)
              _cOstruct :: String
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOsimplifiedName :: (Maybe Node)
              _sOstruct :: String
              _cIannotated :: Node 
              _cIblocks :: (IntMap (Block Node))
              _cIcallMapping :: (IntMap Node)
              _cIconstraints :: (Set Constraint)
              _cIdeclarations :: (Map String Declaration)
              _cIedgeList :: ([UEdge])
              _cIedgeListflow :: ([UEdge])
              _cIexpected :: (Set Constraint)
              _cIexstractFunctions :: Node 
              _cIexstractParameters :: Node 
              _cIfinal :: (Maybe [Label])
              _cIflow :: Flow
              _cIflowpp :: Doc
              _cIinit :: (Maybe Label)
              _cIlabel :: Label
              _cIlabels :: Label
              _cIlabstruct :: (IntMap String)
              _cImapping :: Mapping
              _cInodeList :: ([LNode String])
              _cInodeListflow :: ([LNode String])
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
              _sIedgeListflow :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIexstractFunctions :: Node 
              _sIexstractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIflowpp :: Doc
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sIlabstruct :: (IntMap String)
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodeListflow :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIppcfg :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 12183 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 12188 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 12193 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _cIlabel, ()), (_label, _sIlabel, ())]
                   {-# LINE 12198 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 133 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 12203 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 133 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, fromJust _sIinit)] ++ [(l', _label) | l' <- fromJust _sIfinal]
                   {-# LINE 12208 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 12213 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "while")]
                   {-# LINE 12218 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 12223 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 12228 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 12233 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 12238 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 12243 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   exstractFunctions (While _cIexstractFunctions _sIexstractFunctions) _cIcallMapping
                   {-# LINE 12248 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cIconstraints
                   {-# LINE 12253 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.fromList [(_cIlabel :==: S.singleton TyBool)] `S.union` _cIexpected
                   {-# LINE 12258 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 12263 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _cInodes
                   {-# LINE 12268 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 12273 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "while (" >|< _cIpp >|< text ") {" >-< indent 4 _sIpp >-< text "}"
                   {-# LINE 12278 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup33 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_cOlabels,_) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup33
                   {-# LINE 12285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup33
                   {-# LINE 12290 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 182 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIblocks `IM.union` _sIblocks
                   {-# LINE 12295 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 12300 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 12305 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIdeclarations `M.union` _sIdeclarations
                   {-# LINE 12310 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cIedgeList ++ _sIedgeList
                   {-# LINE 12315 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeListflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIedgeListflow ++ _sIedgeListflow
                   {-# LINE 12320 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 133 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIflow ++ _sIflow
                   {-# LINE 12325 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflowpp =
                  ({-# LINE 14 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIflowpp >|< _sIflowpp
                   {-# LINE 12330 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 12335 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cInodeList ++ _sInodeList
                   {-# LINE 12340 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeListflow =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cInodeListflow ++ _sInodeListflow
                   {-# LINE 12345 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 12350 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 12355 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIppcfg >|< _sIppcfg
                   {-# LINE 12360 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIannotated _sIannotated
                   {-# LINE 12365 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIexstractFunctions _sIexstractFunctions
                   {-# LINE 12370 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIexstractParameters _sIexstractParameters
                   {-# LINE 12375 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIremoved _sIremoved
                   {-# LINE 12380 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  While _cIself _sIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIsimplified _sIsimplified
                   {-# LINE 12387 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 12392 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 12397 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 12402 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 12409 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 12414 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 12419 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12424 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12429 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 12434 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12439 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 12444 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12449 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12454 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIlabels
                   {-# LINE 12459 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cImapping
                   {-# LINE 12464 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12469 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 12474 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _cIannotated,_cIblocks,_cIcallMapping,_cIconstraints,_cIdeclarations,_cIedgeList,_cIedgeListflow,_cIexpected,_cIexstractFunctions,_cIexstractParameters,_cIfinal,_cIflow,_cIflowpp,_cIinit,_cIlabel,_cIlabels,_cIlabstruct,_cImapping,_cInodeList,_cInodeListflow,_cInodes,_cIparamMapping,_cIpp,_cIppcfg,_cIremoved,_cIself,_cIsimplified,_cIwarnings) =
                  c_ _cOdeclaration _cOdeclarations' _cOlabels _cOmapping _cOsimplifiedName _cOstruct 
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIdeclarations,_sIedgeList,_sIedgeListflow,_sIexpected,_sIexstractFunctions,_sIexstractParameters,_sIfinal,_sIflow,_sIflowpp,_sIinit,_sIlabel,_sIlabels,_sIlabstruct,_sImapping,_sInodeList,_sInodeListflow,_sInodes,_sIparamMapping,_sIpp,_sIppcfg,_sIremoved,_sIself,_sIsimplified,_sIwarnings) =
                  s_ _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOsimplifiedName _sOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOedgeListflow,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOflowpp,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodeListflow,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
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
data Syn_OptionalString  = Syn_OptionalString {annotated_Syn_OptionalString :: OptionalString ,exstractFunctions_Syn_OptionalString :: OptionalString ,exstractParameters_Syn_OptionalString :: OptionalString ,paramMapping_Syn_OptionalString :: (IntMap Node),pp_Syn_OptionalString :: Doc,removed_Syn_OptionalString :: OptionalString ,self_Syn_OptionalString :: OptionalString ,simplified_Syn_OptionalString :: OptionalString ,value_Syn_OptionalString :: String}
wrap_OptionalString :: T_OptionalString  ->
                       Inh_OptionalString  ->
                       Syn_OptionalString 
wrap_OptionalString sem (Inh_OptionalString )  =
    (let ( _lhsOannotated,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOvalue) = sem 
     in  (Syn_OptionalString _lhsOannotated _lhsOexstractFunctions _lhsOexstractParameters _lhsOparamMapping _lhsOpp _lhsOremoved _lhsOself _lhsOsimplified _lhsOvalue ))
sem_OptionalString_None :: T_OptionalString 
sem_OptionalString_None  =
    (let _lhsOvalue :: String
         _lhsOpp :: Doc
         _lhsOparamMapping :: (IntMap Node)
         _lhsOannotated :: OptionalString 
         _lhsOexstractFunctions :: OptionalString 
         _lhsOexstractParameters :: OptionalString 
         _lhsOremoved :: OptionalString 
         _lhsOself :: OptionalString 
         _lhsOsimplified :: OptionalString 
         _lhsOvalue =
             ({-# LINE 54 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              ""
              {-# LINE 12516 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
              text "\"\""
              {-# LINE 12521 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOparamMapping =
             ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              IM.empty
              {-# LINE 12526 "src/MF/Language/PHP/AG.hs" #-}
              )
         _annotated =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12531 "src/MF/Language/PHP/AG.hs" #-}
              )
         _exstractFunctions =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12536 "src/MF/Language/PHP/AG.hs" #-}
              )
         _exstractParameters =
             ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12541 "src/MF/Language/PHP/AG.hs" #-}
              )
         _removed =
             ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12546 "src/MF/Language/PHP/AG.hs" #-}
              )
         _self =
             None
         _simplified =
             ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12553 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOannotated =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _annotated
              {-# LINE 12558 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOexstractFunctions =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _exstractFunctions
              {-# LINE 12563 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOexstractParameters =
             ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _exstractParameters
              {-# LINE 12568 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOremoved =
             ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _removed
              {-# LINE 12573 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOself =
             _self
         _lhsOsimplified =
             ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _simplified
              {-# LINE 12580 "src/MF/Language/PHP/AG.hs" #-}
              )
     in  ( _lhsOannotated,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOvalue))
sem_OptionalString_Some :: String ->
                           T_OptionalString 
sem_OptionalString_Some value_  =
    (let _lhsOvalue :: String
         _lhsOpp :: Doc
         _lhsOparamMapping :: (IntMap Node)
         _lhsOannotated :: OptionalString 
         _lhsOexstractFunctions :: OptionalString 
         _lhsOexstractParameters :: OptionalString 
         _lhsOremoved :: OptionalString 
         _lhsOself :: OptionalString 
         _lhsOsimplified :: OptionalString 
         _lhsOvalue =
             ({-# LINE 52 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              value_
              {-# LINE 12598 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
              text "\"" >|< text value_ >|< text "\""
              {-# LINE 12603 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOparamMapping =
             ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              IM.empty
              {-# LINE 12608 "src/MF/Language/PHP/AG.hs" #-}
              )
         _annotated =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12613 "src/MF/Language/PHP/AG.hs" #-}
              )
         _exstractFunctions =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12618 "src/MF/Language/PHP/AG.hs" #-}
              )
         _exstractParameters =
             ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12623 "src/MF/Language/PHP/AG.hs" #-}
              )
         _removed =
             ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12628 "src/MF/Language/PHP/AG.hs" #-}
              )
         _self =
             Some value_
         _simplified =
             ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12635 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOannotated =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _annotated
              {-# LINE 12640 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOexstractFunctions =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _exstractFunctions
              {-# LINE 12645 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOexstractParameters =
             ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _exstractParameters
              {-# LINE 12650 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOremoved =
             ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _removed
              {-# LINE 12655 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOself =
             _self
         _lhsOsimplified =
             ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _simplified
              {-# LINE 12662 "src/MF/Language/PHP/AG.hs" #-}
              )
     in  ( _lhsOannotated,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOvalue))
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
data Syn_ParamList  = Syn_ParamList {annotated_Syn_ParamList :: ParamList ,callMapping_Syn_ParamList :: (IntMap Node),declarations_Syn_ParamList :: (Map String Declaration),exstractFunctions_Syn_ParamList :: ParamList ,exstractParameters_Syn_ParamList :: ParamList ,label_Syn_ParamList :: Label,labels_Syn_ParamList :: Label,mapping_Syn_ParamList :: Mapping,nodes_Syn_ParamList :: (IntMap Node),paramMapping_Syn_ParamList :: (IntMap Node),pp_Syn_ParamList :: Doc,removed_Syn_ParamList :: ParamList ,self_Syn_ParamList :: ParamList ,simplified_Syn_ParamList :: ParamList }
wrap_ParamList :: T_ParamList  ->
                  Inh_ParamList  ->
                  Syn_ParamList 
wrap_ParamList sem (Inh_ParamList _lhsIdeclaration _lhsIdeclarations' _lhsIlabels _lhsImapping _lhsIsimplifiedName )  =
    (let ( _lhsOannotated,_lhsOcallMapping,_lhsOdeclarations,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified) = sem _lhsIdeclaration _lhsIdeclarations' _lhsIlabels _lhsImapping _lhsIsimplifiedName 
     in  (Syn_ParamList _lhsOannotated _lhsOcallMapping _lhsOdeclarations _lhsOexstractFunctions _lhsOexstractParameters _lhsOlabel _lhsOlabels _lhsOmapping _lhsOnodes _lhsOparamMapping _lhsOpp _lhsOremoved _lhsOself _lhsOsimplified ))
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
              _lhsOexstractFunctions :: ParamList 
              _lhsOexstractParameters :: ParamList 
              _lhsOremoved :: ParamList 
              _lhsOself :: ParamList 
              _lhsOsimplified :: ParamList 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _hdOdeclaration :: Declaration
              _hdOdeclarations' :: (Map String Declaration)
              _hdOmapping :: Mapping
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
              _hdIedgeListflow :: ([UEdge])
              _hdIexpected :: (Set Constraint)
              _hdIexstractFunctions :: Node 
              _hdIexstractParameters :: Node 
              _hdIfinal :: (Maybe [Label])
              _hdIflow :: Flow
              _hdIflowpp :: Doc
              _hdIinit :: (Maybe Label)
              _hdIlabel :: Label
              _hdIlabels :: Label
              _hdIlabstruct :: (IntMap String)
              _hdImapping :: Mapping
              _hdInodeList :: ([LNode String])
              _hdInodeListflow :: ([LNode String])
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
              _tlIexstractFunctions :: ParamList 
              _tlIexstractParameters :: ParamList 
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
                  ({-# LINE 48 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 12768 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _hdIpp >|< text "," >|< _tlIpp
                   {-# LINE 12773 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup34 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_hdOlabels,_) =
                  ({-# LINE 47 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup34
                   {-# LINE 12780 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 47 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup34
                   {-# LINE 12785 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _hdIcallMapping `IM.union` _tlIcallMapping
                   {-# LINE 12790 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdIdeclarations `M.union` _tlIdeclarations
                   {-# LINE 12795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdInodes `IM.union` _tlInodes
                   {-# LINE 12800 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _hdIparamMapping `IM.union` _tlIparamMapping
                   {-# LINE 12805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIannotated _tlIannotated
                   {-# LINE 12810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIexstractFunctions _tlIexstractFunctions
                   {-# LINE 12815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIexstractParameters _tlIexstractParameters
                   {-# LINE 12820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIremoved _tlIremoved
                   {-# LINE 12825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIsimplified _tlIsimplified
                   {-# LINE 12832 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 12837 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 12842 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 12847 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 12852 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 12859 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _tlIlabels
                   {-# LINE 12864 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _tlImapping
                   {-# LINE 12869 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12874 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12879 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 12884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOstruct =
                  ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   error "missing rule: ParamList.Cons.hd.struct"
                   {-# LINE 12894 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOdeclaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12899 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12904 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOlabels =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdIlabels
                   {-# LINE 12909 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOmapping =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _hdImapping
                   {-# LINE 12914 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12919 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _hdIannotated,_hdIblocks,_hdIcallMapping,_hdIconstraints,_hdIdeclarations,_hdIedgeList,_hdIedgeListflow,_hdIexpected,_hdIexstractFunctions,_hdIexstractParameters,_hdIfinal,_hdIflow,_hdIflowpp,_hdIinit,_hdIlabel,_hdIlabels,_hdIlabstruct,_hdImapping,_hdInodeList,_hdInodeListflow,_hdInodes,_hdIparamMapping,_hdIpp,_hdIppcfg,_hdIremoved,_hdIself,_hdIsimplified,_hdIwarnings) =
                  hd_ _hdOdeclaration _hdOdeclarations' _hdOlabels _hdOmapping _hdOsimplifiedName _hdOstruct 
              ( _tlIannotated,_tlIcallMapping,_tlIdeclarations,_tlIexstractFunctions,_tlIexstractParameters,_tlIlabel,_tlIlabels,_tlImapping,_tlInodes,_tlIparamMapping,_tlIpp,_tlIremoved,_tlIself,_tlIsimplified) =
                  tl_ _tlOdeclaration _tlOdeclarations' _tlOlabels _tlOmapping _tlOsimplifiedName 
          in  ( _lhsOannotated,_lhsOcallMapping,_lhsOdeclarations,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified)))
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
              _lhsOexstractFunctions :: ParamList 
              _lhsOexstractParameters :: ParamList 
              _lhsOremoved :: ParamList 
              _lhsOself :: ParamList 
              _lhsOsimplified :: ParamList 
              _lhsOmapping :: Mapping
              _lhsOlabel =
                  ({-# LINE 48 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 12952 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 12957 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup35 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 47 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup35
                   {-# LINE 12964 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 47 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup35
                   {-# LINE 12969 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 12974 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 160 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 12979 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 12984 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 12989 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 12994 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 12999 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13004 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13009 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  []
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13016 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 13021 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 13026 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 13031 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 13036 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 13043 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 13048 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOcallMapping,_lhsOdeclarations,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified)))