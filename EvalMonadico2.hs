module EvalMonadico2 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Text.Read (readMaybe)
import Control.Monad.Cont (MonadIO (liftIO))

-- Estados
type Env = [(Variable, Double)]

-- Estado Nulo
initState :: Env
initState = []

-- Mónada estado-error-tick
newtype StateErrorTick a = StateErrorTick { runStateErrorTick :: Env -> IO (Maybe (a, Env, Int)) }
instance Monad StateErrorTick where
    return x = StateErrorTick (\s -> return (Just (x, s, 0)))
    m >>= f = StateErrorTick (\s -> do 
        result <- runStateErrorTick m s
        case result of
            Just (v, s', t) -> do
                result' <- runStateErrorTick (f v) s'
                case result' of
                    Just (v', s'', t') -> return (Just (v', s'', t + t'))
                    Nothing -> return Nothing
            Nothing -> return Nothing)

-- Clase para representar monadas con estado de variables
class Monad m => MonadState m where
    lookfor :: Variable -> m Double
    update :: Variable -> Double -> m ()
instance MonadState StateErrorTick where
    lookfor var = StateErrorTick (\s -> return (maybe Nothing (\v -> Just (v, s, 0)) (lookfor' var s)))
                  where lookfor' var []               = Nothing
                        lookfor' var ((var', val):ss) | var == var' = Just val
                                                      | otherwise   = lookfor' var ss
    update var val = StateErrorTick (\s -> return (Just ((), update' var val s, 0)))
                     where update' var val [] = [(var, val)]
                           update' var val ((var', val'):ss) | var == var' = (var, val):ss
                                                             | otherwise   = (var', val'):(update' var val ss)

class Monad m => MonadError m where
    throw :: m a

instance MonadError StateErrorTick where
    throw = StateErrorTick (\_ -> return Nothing)

-- Clase para representar monadas que cuentan operaciones
class Monad m => MonadTick m where
    tick :: m ()

instance MonadTick StateErrorTick where
    tick = StateErrorTick (\s -> return (Just ((), s, 1)))

-- Para calmar al GHC
instance Functor StateErrorTick where
    fmap = liftM

instance Applicative StateErrorTick where
    pure = return
    (<*>) = ap

instance MonadIO StateErrorTick where
    liftIO ioAction = StateErrorTick (\s -> do
        result <- ioAction
        return (Just (result, s, 0)))

class Monad m => MonadTicOp m where
    ticOp :: Variable -> m ()
    initCount :: Variable -> m ()
    existKey :: Variable -> m Bool

instance MonadTicOp StateErrorTick where
    ticOp op = do exists <- existKey op
                  if exists then do val <- lookfor op
                                    update op (val + 1.0)
                                 else return ()
    initCount op = do exists <- existKey op
                      if not exists then update op 0 else return ()
    existKey op = do result <- StateErrorTick (\s -> return (Just (lookup op s /= Nothing, s, 0)))
                     return result

-- Evalua un programa en el estado nulo
eval :: Comm -> IO (Env, Int)
eval p = do
    result <- runStateErrorTick (evalComm p) initState
    case result of
        Just (v, s, t) -> return (s, t)
        Nothing        -> error "ERROR!"

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadTick m, MonadIO m, MonadTicOp m) => Comm -> m ()
evalComm Skip = return ()
evalComm (Let v e) = do val <- evalExp e
                        ticOp ":="
                        update v val
evalComm (Seq l r) = do evalComm l
                        evalComm r
evalComm (Cond b tc fc) = do bval <- evalBoolExp b
                             ticOp "if"
                             if bval then evalComm tc 
                             else evalComm fc
evalComm (Repeat c b) = do ticOp "repeat"
                           evalComm (Seq c (Cond b Skip (Repeat c b)))
evalComm (Input v) = do liftIO (putStrLn "Waiting for a value...")
                        input <- liftIO getLine
                        ticOp "input"
                        case readMaybe input :: Maybe Double of
                            Just number -> update v number
                            Nothing -> throw
evalComm (Print t e) = do val <- evalExp e
                          ticOp "print"
                          liftIO (putStrLn (case t of
                                  Just txt -> txt ++ " " ++ show val
                                  Nothing  -> show val))
evalComm (Tic (Var variable)) = do initCount variable
                          
-- Evalua una expresion
evalExp :: (MonadState m, MonadError m, MonadTick m, MonadTicOp m) => Exp -> m Double
evalExp (Const n) = return (fromInteger n)
evalExp (DoubleConst f) = return f
evalExp (Var v) = do val <- lookfor v
                     return val
evalExp (UMinus e) = do val <- evalExp e
                        ticOp "-"
                        return (negate val)
evalExp (Plus l r) = do lval <- evalExp l
                        rval <- evalExp r
                        tick
                        ticOp "+"
                        return (lval + rval)
evalExp (Minus l r) = do lval <- evalExp l
                         rval <- evalExp r
                         tick
                         ticOp "-"
                         return (lval - rval)
evalExp (Times l r) = do lval <- evalExp l
                         rval <- evalExp r
                         tick
                         ticOp "*"
                         return (lval * rval)
evalExp (Div l r) = do lval <- evalExp l
                       rval <- evalExp r
                       ticOp "/"
                       if rval == 0 then throw
                       else do tick
                               return (lval / rval)
evalExp (Sin e) = do val <- evalExp e
                     ticOp "sin"
                     return (sin (val * pi / 180))
evalExp (Cos e) = do val <- evalExp e
                     ticOp "cos"
                     return (cos (val * pi / 180))
evalExp (Tan e) = do val <- evalExp e
                     ticOp "tan"
                     return (tan (val * pi / 180))
evalExp (Ceil e) = do val <- evalExp e
                      ticOp "ceil"
                      return (fromIntegral (ceiling val))
evalExp (Floor e) = do val <- evalExp e
                       ticOp "floor"
                       return (fromIntegral (floor val))
evalExp (Round e f) = do 
    expo <- evalExp e
    val <- evalExp f
    let expo' = max 0 (floor expo)
        factor = 10 ^ expo'
    return (fromIntegral (round (val * factor)) / factor)
             
evalExp Pi = do ticOp "pi"
                return pi

-- Evalua una expresion booleana
evalBoolExp :: (MonadState m, MonadError m, MonadTick m, MonadTicOp m) => BoolExp -> m Bool
evalBoolExp BTrue = return True
evalBoolExp BFalse = return False
evalBoolExp (Eq l r) = do lval <- evalExp l
                          rval <- evalExp r
                          ticOp "="
                          return (lval == rval)
evalBoolExp (Lt l r) = do lval <- evalExp l
                          rval <- evalExp r
                          ticOp "<"
                          return (lval < rval)
evalBoolExp (Gt l r) = do lval <- evalExp l
                          rval <- evalExp r
                          ticOp ">"
                          return (lval > rval)
evalBoolExp (And l r) = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           ticOp "&"
                           return (lval && rval)
evalBoolExp (Or l r) = do lval <- evalBoolExp l
                          rval <- evalBoolExp r
                          ticOp "|"
                          return (lval || rval)
evalBoolExp (Not b) = do bval <- evalBoolExp b
                         ticOp "~"
                         return (not bval)
