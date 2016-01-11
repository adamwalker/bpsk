{-# LANGUAGE RecordWildCards, ScopedTypeVariables, FlexibleContexts #-}

import Control.Monad.Trans.Either
import Data.Word
import Data.Int
import Data.Complex
import Foreign.C.Types
import Data.Maybe
import Control.Monad
import Foreign.Storable

import Pipes as P
import qualified Pipes.Prelude as P
import Options.Applicative
import qualified Data.Vector.Storable as VS 
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Generic as VG 
import Graphics.Rendering.OpenGL (GLfloat)
import Data.Tuple
import qualified Control.Arrow as A

import SDR.Util as U
import SDR.RTLSDRStream
import SDR.FFT
import SDR.Plot
import SDR.ArgUtils
import SDR.PipeUtils
import SDR.FilterDesign
import SDR.CPUID
import SDR.Filter
import SDR.VectorUtils
import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Util

data Options = Options {
    frequency    :: Word32,
    sampleRate   :: Word32,
    gain         :: Maybe Int32,
    fftSize      :: Maybe Int,
    windowWidth  :: Maybe Int,
    windowHeight :: Maybe Int,
    rows         :: Maybe Int,
    colorMap     :: Maybe [GLfloat]
}

parseColorMap :: ReadM [GLfloat]
parseColorMap = eitherReader func
    where
    func "jet"     = return jet
    func "jet_mod" = return jet_mod
    func "hot"     = return hot
    func "bw"      = return bw
    func "wb"      = return wb
    func arg       = Left $ "Cannot parse colour map: `" ++ arg ++ "'"

optParser :: Parser Options
optParser = Options 
          <$> option (fmap fromIntegral parseSize) (
                 long "frequency"  
              <> short 'f' 
              <> metavar "FREQUENCY" 
              <> help "Frequency to tune to"
              )
          <*> option (fmap fromIntegral parseSize) (
                 long "samplerate" 
              <> short 'r' 
              <> metavar "RATE" 
              <> help "Sample rate"
              )
          <*> optional (option auto (
                 long "gain" 
              <> short 'g' 
              <> metavar "GAIN" 
              <> help "Tuner gain"
              ))
          <*> optional (option (fmap fromIntegral parseSize) (
                 long "size" 
              <> short 's' 
              <> metavar "SIZE" 
              <> help "FFT bin size. Default is 8192."
              ))
          <*> optional (option auto (
                 long "width" 
              <> short 'w' 
              <> metavar "WIDTH" 
              <> help "Window width. Default is 1024."
              ))
          <*> optional (option auto (
                 long "height" 
              <> short 'h' 
              <> metavar "HEIGHT" 
              <> help "Window height. Default is 480."
              ))
          <*> optional (option auto (
                 long "rows" 
              <> short 'r' 
              <> metavar "ROWS" 
              <> help "Number of rows in waterfall. Default is 1000."
              ))
          <*> optional (option parseColorMap (
                 long "colorMap" 
              <> short 'm' 
              <> metavar "COLORMAP" 
              <> help "Waterfall color map. Default is 'jet_mod'."
              ))

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Draw a dynamic waterall plot of the received spectrum using OpenGL" <> header "RTLSDR Waterfall")

(a :+ b) `cdiv` y = (a/y) :+ (b/y)
(a :+ b) `cmul` y = (a*y) :+ (b*y)
normalize x = x `cdiv` magnitude x

pll :: (Num a, RealFloat a, Storable a) => a -> a -> (Complex a, Complex a) -> VS.Vector (Complex a) -> ((Complex a, Complex a), VS.Vector (Complex a)) 
pll mu muAccum state input = A.first snd $ swap $ vUnfoldr (VS.length input) go (0, state)
    where
    go (offset, (state, accumError)) = 
        let corrected    = state * input VS.! offset
            err          = {- realPart corrected * imagPart corrected -} atan (imagPart corrected / realPart corrected)
            correction   = (-1) * mu * err
            stateNext    = cis correction * state * accumError
        in (corrected, (offset + 1, (normalize stateNext, normalize $ accumError * cis ((-1) * muAccum * err))))

thing :: (Monad m, MonadTrans t, Monad (t m)) => m (t m a) -> t m a
thing = join . lift

coarseFreq :: Int -> Pipe (VS.Vector (Complex CDouble)) Int IO ()
coarseFreq size =   P.map (VG.map (\x -> x * x)) 
                >-> P.map (VG.zipWith (flip mult) (hanning size :: VS.Vector CDouble)) 
                >-> thing (fftw size) 
                >-> P.map (VG.map magnitude) 
                >-> P.map VG.maxIndex

freqCorrection :: (VG.Vector v (Complex n), Floating n, Num n)
               => Int           -- ^ Length of vector
               -> n             -- ^ Frequency
               -> v (Complex n)
freqCorrection size frequency = VG.generate size func
    where
    func idx = cis ((fromIntegral idx) * frequency)

correctFreq :: Int -> Pipe (VS.Vector (Complex CDouble)) (VS.Vector (Complex CDouble)) IO ()
correctFreq size = do
    theFFT <- lift $ fftw' size
    let func input = do
            let squared  = VS.map (\x -> x*x) input
                windowed = VS.zipWith (flip mult) window squared
            fftd <- theFFT windowed
            let maxFreq = VG.maxIndex (VG.map magnitude fftd) `quot` 2
            return $ VG.zipWith mult (Main.freqCorrection size (-1 * (fromIntegral maxFreq / fromIntegral size) * 2 * pi)) input
        window = hanning size :: VS.Vector CDouble
    P.mapM func

doIt Options{..} = do
    res <- lift setupGLFW
    unless res (left "Unable to initilize GLFW")

    let fftSize' =  fromMaybe 8192 fftSize
        window   =  hanning fftSize' :: VS.Vector Float
    str          <- sdrStream ((defaultRTLSDRParams frequency sampleRate) {tunerGain = gain}) 1 (fromIntegral $ fftSize' * 2)
    --rfFFT        <- lift $ fftw fftSize'
    --rfSpectrum   <- plotWaterfall (fromMaybe 1024 windowWidth) (fromMaybe 480 windowHeight) fftSize' (fromMaybe 1000 rows) (fromMaybe jet_mod colorMap)
    --rfSpectrum   <- plotFill (maybe 1024 id windowWidth) (maybe 480 id windowHeight) fftSize' (maybe jet_mod id colorMap)
    --rfSpectrum   <- plotLine (fromMaybe 1024 windowWidth) (fromMaybe 480 windowHeight) fftSize' fftSize'

    let coeffs :: [Float] = map (*0.125) $ srrc 128 8 0.25
    info   <- lift getCPUInfo
    matchedFilter :: Resampler IO VS.Vector VSM.MVector (Complex Float) <- lift $ fastResamplerC info 1 1 coeffs

    lift $ runEffect $   str 
                     >-> P.map interleavedIQUnsignedByteToFloat
                     >-> pMapAccum (agc 0.001 0.2) 1
                     -- >-> firResampler matchedFilter 8192
                     -- >-> pMapAccum (pll 1 0.2) (1, 1)
                     
                     >-> P.map (VG.map (cplxMap (realToFrac :: Float -> CDouble)))
                     >-> correctFreq 8192
                     >-> coarseFreq 8192
                     >-> P.print

                     -- >-> P.map (VG.map (\x -> x*x))
                     -- >-> P.map (VG.zipWith (flip mult) window . VG.zipWith mult (halfBandUp fftSize')) 
                     -- >-> P.map (VG.map (cplxMap (realToFrac :: Float -> CDouble)))
                     -- >-> rfFFT 
                     -- >-> P.map (VG.map ((* (32 / fromIntegral fftSize')) . realToFrac . magnitude)) 
                     -- >-> rfSpectrum 

main = execParser opt >>= eitherT putStrLn return . doIt

