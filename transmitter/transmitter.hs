{-# LANGUAGE RecordWildCards, ScopedTypeVariables, FlexibleContexts, GADTs #-}

import Control.Monad.Trans.Either
import Data.Complex
import Foreign.C.Types
import Data.Maybe
import Control.Monad
import Data.Bits
import Control.Monad.ST
import Data.Word
import qualified System.Random.MWC as R
import Control.Monad.Primitive

import Pipes as P
import qualified Pipes.Prelude as P
import Options.Applicative
import qualified Data.Vector.Storable as VS 
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Generic as VG 
import qualified Data.Vector.Generic.Mutable as VGM

import SDR.Util as U
import SDR.ArgUtils
import SDR.FilterDesign
import SDR.VectorUtils
import SDR.Filter
import SDR.CPUID

import LibBladeRF.LibBladeRF
import LibBladeRF.Types
import LibBladeRF.Pipes
import LibBladeRF.Utils

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

data Options = Options {
    frequency    :: Int,
    sampleRate   :: Int,
    bandwidth    :: Maybe Int
}

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
          <*> optional (option (fmap fromIntegral parseSize) (
                 long "bandwidth" 
              <> short 'b' 
              <> metavar "BW" 
              <> help "Bandwidth. Defaults to sample rate / 2"
              ))

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Transmit a sine wave" <> header "BladeRF Transmit")

--producer2 = forever $ yield $ VS.replicate 8192 1024

doIt Options{..} = do

    let coeffs :: [Float] = map (*0.125) $ srrc 128 16 0.25
    lift $ plotFrequency $ map realToFrac coeffs
    lift $ toFile def "srrc.png" $ do
        layout_title .= "title"
        plot (line "Raised cosine" [zip (map (fromIntegral :: Int -> Float) [(-100)..100]) ((srrc 100 10 1) :: [Float])])

    info   <- lift getCPUInfo
    interp :: Resampler IO VS.Vector VSM.MVector (Complex Float) <- lift $ fastResamplerC info 16 1 coeffs

    --let producer = streamString ((map (toEnum . fromEnum) "hello, this is a test. It is a very good test string. One of my favourites, actually. If I make it long enough ao the spectral lines might not be visible and that would be totaly awesome. SOME CAPS FOR FUNNNNNNNN. YAYYYYYYYYY IM PAIGE") :: [Word8]) 8192 >-> P.map (VG.map $ \x -> x :+ 0) >-> firResampler interp 8192 >-> P.map complexFloatToInterleavedIQSignedWord 
    let producer = streamRandom 8192 >-> P.map (VG.map $ \x -> x :+ 0) >-> firResampler interp 8192 >-> P.map complexFloatToInterleavedIQSignedWord 
    res  <- lift $ P.toListM $ producer >-> P.take 10 
    --lift $ print res

    dev <- lift openBladeRF
    sink <- bladeRFSink dev (BladeRFTxConfig frequency sampleRate (fromMaybe (sampleRate `quot` 2) bandwidth) (-10) 20)
    lift $ runEffect $ producer >->  sink

    lift $ bladeRFEnableModule dev MODULE_TX False
    return ()

main = execParser opt >>= eitherT putStrLn return . doIt

