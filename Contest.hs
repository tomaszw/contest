import qualified Data.Map as M
import Data.Maybe

type ID = Int
type Circuit = [Gate]
data CircuitState = CircuitState { gateMap :: M.Map ID GateState
                                 , delayedInps :: [SignalApp] }

data Conn = Conn { cid :: ID, destWire :: Wire, isReverse :: Bool }

data Gate = Gate {    gid :: ID
                    , cinput_l :: Conn
                    , cinput_r :: Conn
                    , coutput_l :: Conn
                    , coutput_r :: Conn }

data GateState = GateState { inputs  :: (Val,Val)
                           , outputs :: (Val,Val) }

data SignalApp = SignalApp ID Wire Val
data Val = V0 | V1 | V2
data Wire = L | R
type Time = Int
toNum V0 = 0
toNum V1 = 1
toNum V2 = 2
ofNum 0 = V0
ofNum 1 = V1
ofNum 2 = V2
instance Show Val where
    show V0 = "0"
    show V1 = "1"
    show V2 = "2"

signals = [V0, V1, V2]

extGateID :: ID
extGateID = -1

sampleCircuit :: Circuit
sampleCircuit =
    [ Gate { gid = 0
           , cinput_l  = Conn 0 L True
           , cinput_r  = Conn extGateID L False
           , coutput_l = Conn 0 L True
           , coutput_r = Conn extGateID L False }
    ]

fac n = product [1..n]

sampleCircuit2 :: Circuit
sampleCircuit2 =
    [ Gate { gid = 0
           , cinput_l  = Conn 1 L False
           , cinput_r  = Conn extGateID L False
           , coutput_l = Conn 1 L False
           , coutput_r = Conn 1 R False }
    , Gate { gid = 1
           , cinput_l  = Conn 0 L False
           , cinput_r  = Conn 0 R False
           , coutput_l = Conn 0 L True
           , coutput_r = Conn extGateID L False }
    ]
             

initialState :: Circuit -> CircuitState
initialState circuit =
    CircuitState { gateMap     = zeroMap
                 , delayedInps = [] }
  where
    zeroMap = M.fromList $ zip (gateIDs circuit) (map zeroG $ gateIDs circuit)
    zeroG _ = GateState (V0,V0) (V0,V0)

isLInputExt :: Gate -> Bool
isLInputExt g = (cid . cinput_l $ g) == extGateID
isRInputExt :: Gate -> Bool
isRInputExt g = (cid . cinput_r $ g) == extGateID
isLOutputExt :: Gate -> Bool
isLOutputExt g = (cid . coutput_l $ g) == extGateID
isROutputExt :: Gate -> Bool
isROutputExt g = (cid . coutput_r $ g) == extGateID

gateIDs :: Circuit -> [ID]
gateIDs circuit = map gid circuit

getLOutputDestApp :: Gate -> (Val -> SignalApp)
getLOutputDestApp g = SignalApp (cid . coutput_l $ g) (destWire . coutput_l $ g)

getROutputDestApp :: Gate -> (Val -> SignalApp)
getROutputDestApp g = SignalApp (cid . coutput_r $ g) (destWire . coutput_r $ g)

getLOutputDest :: Gate -> ID
getLOutputDest g = cid . coutput_l $ g
getROutputDest :: Gate -> ID
getROutputDest g = cid . coutput_r $ g

isLOutputDelayed = not . isLOutputCurrent
isROutputDelayed = not . isROutputCurrent

isLOutputCurrent :: Gate -> Bool
isLOutputCurrent g = let c = coutput_l g in
                     not (isReverse c)

isROutputCurrent :: Gate -> Bool
isROutputCurrent g = let c = coutput_r g in
                     not (isReverse c)

applySignal :: GateState -> SignalApp -> GateState
applySignal gs (SignalApp _ L v) = let (_,bi) = inputs gs in
                                   GateState (v,bi) (outputs gs)
applySignal gs (SignalApp _ R v) = let (ai,_) = inputs gs in
                                   GateState (ai,v) (outputs gs)

applySignal2 :: CircuitState -> SignalApp -> CircuitState
applySignal2 cs (SignalApp (-1) _ _) = cs
applySignal2 cs app@(SignalApp gid wire v) =
    CircuitState { gateMap = M.insert gid gate_state' gateM
                 , delayedInps = delayedInps cs }
  where
    gate_state :: GateState
    gate_state  = fromJust $ M.lookup gid gateM

    gate_state' :: GateState
    gate_state' = applySignal gate_state app

    gateM :: M.Map ID GateState 
    gateM = gateMap cs
    
--simGate :: Gate -> (Val, Val) -> (Val, Val)
--simGate = ?

getGateDef :: Circuit -> ID -> Gate
getGateDef circuit (-1) =
    Gate { gid = -1
         , cinput_l  = Conn extGateID L False
         , cinput_r  = Conn extGateID L False
         , coutput_l = Conn extGateID L False
         , coutput_r = Conn extGateID L False }

getGateDef circuit i =
    case filter (\g -> gid g == i) circuit of
      (x:_) -> x
      _     -> error $ "whatthefuck " ++ show i

gateFun (V0,V0) = (V0,V2)
gateFun (V0,V1) = (V2,V2)
gateFun (V1,V0) = (V1,V2)
gateFun (V1,V1) = (V0,V0)
gateFun (V0,V2) = (V1,V2)
gateFun (V1,V2) = (V2,V1)
gateFun (V2,V2) = (V0,V0)
gateFun (V2,V0) = (V2,V2)
gateFun (V2,V1) = (V1,V1)

computeGates :: CircuitState -> CircuitState
computeGates cs =
    cs { gateMap = M.map computeGate gateM0 }
  where computeGate gs = gs { outputs = gateFun (inputs gs) }
        gateM0 = gateMap cs
 
simCircuit :: Circuit -> CircuitState -> CircuitState
simCircuit circuit circuitState =
    let s' = applyDelayed circuitState in
    computeGates $ applyCurrent s'
  where
    apply = applySignal2
    applyDelayed s = foldl apply s (delayed     s)
    applyCurrent s = foldl apply s (currentInps s)
    currentInps  s = concat . map getCurrentInps $ M.toList (gateMap s)
    delayed      s = concat . map getDelayedInps $ M.toList (gateMap s)
    getDelayedInps (-1, _) = []
    getDelayedInps (gid, gs) =
        l ++ r
      where
        g = getGateDef circuit gid
        l = if isLOutputDelayed g
               then [ getLOutputDestApp g (fst . outputs $ gs) ]
               else [ ]
        r = if isROutputDelayed g
               then [ getROutputDestApp g (snd . outputs $ gs) ]
               else [ ]

    getCurrentInps (-1, _) = []
    getCurrentInps (gid, gs) =
        l ++ r
      where
        g = getGateDef circuit gid
        l = if isLOutputCurrent g
               then [ getLOutputDestApp g (fst . outputs $ gs) ]
               else [ ]
        r = if isROutputCurrent g
               then [ getROutputDestApp g (snd . outputs $ gs) ]
               else [ ]

type Input  = [Val]
type Output = [Val]

run :: Circuit -> Input -> Output
run circuit input =
    sim circuit (initialState circuit) input

sim :: Circuit -> CircuitState -> Input -> Output
sim circuit state [] = []
sim circuit state (inp : inps) =
    output' : sim circuit state'  inps
  where
    (state', output') = solve circuit state inp

solve :: Circuit -> CircuitState -> Val -> (CircuitState, Val)
solve circuit state inp =
    let s0  = applyExtInput circuit state inp
        s1  = simCircuit circuit s0
        out = readExtOutput circuit s1
    in
      (s1, out)

readExtOutput :: Circuit -> CircuitState -> Val
readExtOutput circuit state =
    head $ extOutputs
  where
    extOutputs :: [Val]
    extOutputs = concat . M.elems $ M.mapWithKey extOutput (gateMap state)
    extOutput :: ID -> GateState -> [Val]
    extOutput id gs =
        let g = getGateDef circuit id
            l = if isLOutputExt g then [fst . outputs $ gs] else []
            r = if isROutputExt g then [snd . outputs $ gs] else []
        in l ++ r

applyExtInput :: Circuit -> CircuitState -> Val -> CircuitState
applyExtInput circuit state v =
    state { gateMap = M.mapWithKey apply (gateMap state) }
  where
    apply id gs     =
        let g       = getGateDef circuit id
            (l,r)   = inputs gs
            inps'   = if isLInputExt g then (v,r) else (l,r)
            (l',r') = inps'
            inps''  = if isRInputExt g then (l',v) else (l',r')
        in
          GateState { inputs = inps'', outputs = outputs gs }

