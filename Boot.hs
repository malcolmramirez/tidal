:set -XOverloadedStrings
:set prompt ""

import Sound.Tidal.Context

import System.IO (hSetEncoding, stdout, utf8)
hSetEncoding stdout utf8

tidal <- startTidal (superdirtTarget {oLatency = 0.05, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cVerbose = True, cFrameTimespan = 1/20})

:{
let only = (hush >>)
    p = streamReplace tidal
    hush = streamHush tidal
    panic = do hush
               once $ sound "superpanic"
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    unmuteAll = streamUnmuteAll tidal
    unsoloAll = streamUnsoloAll tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal
    first = streamFirst tidal
    asap = once
    nudgeAll = streamNudgeAll tidal
    all = streamAll tidal
    resetCycles = streamResetCycles tidal
    setCycle = streamSetCycle tidal
    setcps = asap . cps
    getcps = streamGetcps tidal
    getnow = streamGetnow tidal
    xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition tidal True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
    jumpMod' i t p = transition tidal True (Sound.Tidal.Transition.jumpMod' t p) i
    mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
    d1 = p 1 . (|< orbit 0)
    d2 = p 2 . (|< orbit 1)
    d3 = p 3 . (|< orbit 2)
    d4 = p 4 . (|< orbit 3)
    d5 = p 5 . (|< orbit 4)
    d6 = p 6 . (|< orbit 5)
    d7 = p 7 . (|< orbit 6)
    d8 = p 8 . (|< orbit 7)
    d9 = p 9 . (|< orbit 8)
    d10 = p 10 . (|< orbit 9)
    d11 = p 11 . (|< orbit 10)
    d12 = p 12 . (|< orbit 11)
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16
    -- fm6
    -- Parameters
    fmmod :: Int -> Int -> Pattern Double -> ControlPattern
    fmegrate :: Int -> Int -> Pattern Double -> ControlPattern
    fmeglevel :: Int -> Int -> Pattern Double -> ControlPattern
    fmampname op = "amp" ++ show op
    fmamp op = pF (fmampname op)
    fmratio op = pF ("ratio" ++ show op)
    fmdetune op = pF ("detune" ++ show op)
    fmmod opa opb = pF ("mod" ++ show opa ++ show opb)
    fmegrate op step = pF ("egrate" ++ show op ++ show step)
    fmeglevel op step = pF ("eglevel" ++ show op ++ show step)
    fmfeedback = pF "feedback"
    -- Array functions
    fmparam :: (Int -> a -> ControlPattern) -> [a] -> ControlPattern
    fmparam function (x:xs) = foldr (#) (function 1 x) (zipWith function [2..] xs)
    fmamps = fmparam fmamp
    fmratios = fmparam fmratio
    fmdetunes = fmparam fmdetune
    fmegrates op = fmparam (fmegrate op)
    fmeglevels op = fmparam (fmeglevel op)
    fmmods opa = fmparam (fmmod opa)
    -- Bus functions
    makeBus name busId pat = pF name pat # pI ("^" ++ name) busId
    makeRecv name busId = pI ("^ ++ name") busId

    mod11bus = makeBus "mod11"
    mod11recv = makeRecv "mod11"
    mod12bus = makeBus "mod12"
    mod12recv = makeRecv "mod12"
    mod13bus = makeBus "mod13"
    mod13recv = makeRecv "mod13"
    mod14bus = makeBus "mod14"
    mod14recv = makeRecv "mod14"
    mod15bus = makeBus "mod15"
    mod15recv = makeRecv "mod15"
    mod16bus = makeBus "mod16"
    mod16recv = makeRecv "mod16"
    mod21bus = makeBus "mod21"
    mod21recv = makeRecv "mod21"
    mod22bus = makeBus "mod22"
    mod22recv = makeRecv "mod22"
    mod23bus = makeBus "mod23"
    mod23recv = makeRecv "mod23"
    mod24bus = makeBus "mod24"
    mod24recv = makeRecv "mod24"
    mod25bus = makeBus "mod25"
    mod25recv = makeRecv "mod25"
    mod26bus = makeBus "mod26"
    mod26recv = makeRecv "mod26"
    mod31bus = makeBus "mod31"
    mod31recv = makeRecv "mod31"
    mod32bus = makeBus "mod32"
    mod32recv = makeRecv "mod32"
    mod33bus = makeBus "mod33"
    mod33recv = makeRecv "mod33"
    mod34bus = makeBus "mod34"
    mod34recv = makeRecv "mod34"
    mod35bus = makeBus "mod35"
    mod35recv = makeRecv "mod35"
    mod36bus = makeBus "mod36"
    mod36recv = makeRecv "mod36"
    mod41bus = makeBus "mod41"
    mod41recv = makeRecv "mod41"
    mod42bus = makeBus "mod42"
    mod42recv = makeRecv "mod42"
    mod43bus = makeBus "mod43"
    mod43recv = makeRecv "mod43"
    mod44bus = makeBus "mod44"
    mod44recv = makeRecv "mod44"
    mod45bus = makeBus "mod45"
    mod45recv = makeRecv "mod45"
    mod46bus = makeBus "mod46"
    mod46recv = makeRecv "mod46"
    mod51bus = makeBus "mod51"
    mod51recv = makeRecv "mod51"
    mod52bus = makeBus "mod52"
    mod52recv = makeRecv "mod52"
    mod53bus = makeBus "mod53"
    mod53recv = makeRecv "mod53"
    mod54bus = makeBus "mod54"
    mod54recv = makeRecv "mod54"
    mod55bus = makeBus "mod55"
    mod55recv = makeRecv "mod55"
    mod56bus = makeBus "mod56"
    mod56recv = makeRecv "mod56"
    mod61bus = makeBus "mod61"
    mod61recv = makeRecv "mod61"
    mod62bus = makeBus "mod62"
    mod62recv = makeRecv "mod62"
    mod63bus = makeBus "mod63"
    mod63recv = makeRecv "mod63"
    mod64bus = makeBus "mod64"
    mod64recv = makeRecv "mod64"
    mod65bus = makeBus "mod65"
    mod65recv = makeRecv "mod65"
    mod66bus = makeBus "mod66"
    mod66recv = makeRecv "mod66"

    eglevel11bus = makeBus "eglevel11"
    eglevel11recv = makeRecv "eglevel11"
    eglevel12bus = makeBus "eglevel12"
    eglevel12recv = makeRecv "eglevel12"
    eglevel13bus = makeBus "eglevel13"
    eglevel13recv = makeRecv "eglevel13"
    eglevel14bus = makeBus "eglevel14"
    eglevel14recv = makeRecv "eglevel14"
    eglevel21bus = makeBus "eglevel21"
    eglevel21recv = makeRecv "eglevel21"
    eglevel22bus = makeBus "eglevel22"
    eglevel22recv = makeRecv "eglevel22"
    eglevel23bus = makeBus "eglevel23"
    eglevel23recv = makeRecv "eglevel23"
    eglevel24bus = makeBus "eglevel24"
    eglevel24recv = makeRecv "eglevel24"
    eglevel31bus = makeBus "eglevel31"
    eglevel31recv = makeRecv "eglevel31"
    eglevel32bus = makeBus "eglevel32"
    eglevel32recv = makeRecv "eglevel32"
    eglevel33bus = makeBus "eglevel33"
    eglevel33recv = makeRecv "eglevel33"
    eglevel34bus = makeBus "eglevel34"
    eglevel34recv = makeRecv "eglevel34"
    eglevel41bus = makeBus "eglevel41"
    eglevel41recv = makeRecv "eglevel41"
    eglevel42bus = makeBus "eglevel42"
    eglevel42recv = makeRecv "eglevel42"
    eglevel43bus = makeBus "eglevel43"
    eglevel43recv = makeRecv "eglevel43"
    eglevel44bus = makeBus "eglevel44"
    eglevel44recv = makeRecv "eglevel44"
    eglevel51bus = makeBus "eglevel51"
    eglevel51recv = makeRecv "eglevel51"
    eglevel52bus = makeBus "eglevel52"
    eglevel52recv = makeRecv "eglevel52"
    eglevel53bus = makeBus "eglevel53"
    eglevel53recv = makeRecv "eglevel53"
    eglevel54bus = makeBus "eglevel54"
    eglevel54recv = makeRecv "eglevel54"
    eglevel61bus = makeBus "eglevel61"
    eglevel61recv = makeRecv "eglevel61"
    eglevel62bus = makeBus "eglevel62"
    eglevel62recv = makeRecv "eglevel62"
    eglevel63bus = makeBus "eglevel63"
    eglevel63recv = makeRecv "eglevel63"
    eglevel64bus = makeBus "eglevel64"
    eglevel64recv = makeRecv "eglevel64"

    egrate11bus = makeBus "egrate11"
    egrate11recv = makeRecv "egrate11"
    egrate12bus = makeBus "egrate12"
    egrate12recv = makeRecv "egrate12"
    egrate13bus = makeBus "egrate13"
    egrate13recv = makeRecv "egrate13"
    egrate14bus = makeBus "egrate14"
    egrate14recv = makeRecv "egrate14"
    egrate21bus = makeBus "egrate21"
    egrate21recv = makeRecv "egrate21"
    egrate22bus = makeBus "egrate22"
    egrate22recv = makeRecv "egrate22"
    egrate23bus = makeBus "egrate23"
    egrate23recv = makeRecv "egrate23"
    egrate24bus = makeBus "egrate24"
    egrate24recv = makeRecv "egrate24"
    egrate31bus = makeBus "egrate31"
    egrate31recv = makeRecv "egrate31"
    egrate32bus = makeBus "egrate32"
    egrate32recv = makeRecv "egrate32"
    egrate33bus = makeBus "egrate33"
    egrate33recv = makeRecv "egrate33"
    egrate34bus = makeBus "egrate34"
    egrate34recv = makeRecv "egrate34"
    egrate41bus = makeBus "egrate41"
    egrate41recv = makeRecv "egrate41"
    egrate42bus = makeBus "egrate42"
    egrate42recv = makeRecv "egrate42"
    egrate43bus = makeBus "egrate43"
    egrate43recv = makeRecv "egrate43"
    egrate44bus = makeBus "egrate44"
    egrate44recv = makeRecv "egrate44"
    egrate51bus = makeBus "egrate51"
    egrate51recv = makeRecv "egrate51"
    egrate52bus = makeBus "egrate52"
    egrate52recv = makeRecv "egrate52"
    egrate53bus = makeBus "egrate53"
    egrate53recv = makeRecv "egrate53"
    egrate54bus = makeBus "egrate54"
    egrate54recv = makeRecv "egrate54"
    egrate61bus = makeBus "egrate61"
    egrate61recv = makeRecv "egrate61"
    egrate62bus = makeBus "egrate62"
    egrate62recv = makeRecv "egrate62"
    egrate63bus = makeBus "egrate63"
    egrate63recv = makeRecv "egrate63"
    egrate64bus = makeBus "egrate64"
    egrate64recv = makeRecv "egrate64"

    amp1bus = makeBus "amp1"
    amp1recv = makeRecv "amp1"
    amp2bus = makeBus "amp2"
    amp2recv = makeRecv "amp2"
    amp3bus = makeBus "amp3"
    amp3recv = makeRecv "amp3"
    amp4bus = makeBus "amp4"
    amp4recv = makeRecv "amp4"
    amp5bus = makeBus "amp5"
    amp5recv = makeRecv "amp5"
    amp6bus = makeBus "amp6"
    amp6recv = makeRecv "amp6"

    ratio1bus = makeBus "ratio1"
    ratio1recv = makeRecv "ratio1"
    ratio2bus = makeBus "ratio2"
    ratio2recv = makeRecv "ratio2"
    ratio3bus = makeBus "ratio3"
    ratio3recv = makeRecv "ratio3"
    ratio4bus = makeBus "ratio4"
    ratio4recv = makeRecv "ratio4"
    ratio5bus = makeBus "ratio5"
    ratio5recv = makeRecv "ratio5"
    ratio6bus = makeBus "ratio6"
    ratio6recv = makeRecv "ratio6"

    detune1bus = makeBus "detune1"
    detune1recv = makeRecv "detune1"
    detune2bus = makeBus "detune2"
    detune2recv = makeRecv "detune2"
    detune3bus = makeBus "detune3"
    detune3recv = makeRecv "detune3"
    detune4bus = makeBus "detune4"
    detune4recv = makeRecv "detune4"
    detune5bus = makeBus "detune5"
    detune5recv = makeRecv "detune5"
    detune6bus = makeBus "detune6"
    detune6recv = makeRecv "detune6"

    lfofreqbus = makeBus "lfofreq"
    lfofreqrecv = makeRecv "lfofreq"

    lfodepthbus = makeBus "lfodepth"
    lfodepthrecv = makeRecv "lfodepth"

    feedbackbus = makeBus "feedback"
    feedbackrecv = makeRecv "feedback"

    lfofreq = pF "lfofreq"
    lfodepth = pF "lfodepth"
:}

:{
let getState = streamGet tidal
    setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetR tidal
    setB = streamSetB tidal
:}

:set prompt "tidal> "
:set prompt-cont ""

default (Pattern String, Integer, Double)
