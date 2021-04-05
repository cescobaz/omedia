
module Binding.Metadata where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Media (Metadata(..))

#include "oexif.h"

peekMetadata :: Ptr a -> IO Metadata
peekMetadata ptr = do
  dateTime <- #{peek Metadata, dateTime} ptr >>= maybePeek peekCString
  subSecTime <- #{peek Metadata, subSecTime} ptr >>= maybePeek peekCString
  offsetTime <- #{peek Metadata, offsetTime} ptr >>= maybePeek peekCString
  
  orientation <- (fmap fromIntegral) <$> (#{peek Metadata, orientation} ptr >>= maybePeek peek :: IO (Maybe CInt))
  
  uniqueCameraModel <- #{peek Metadata, uniqueCameraModel} ptr >>= maybePeek peekCString
  localizedCameraModel <- #{peek Metadata, localizedCameraModel} ptr >>= maybePeek peekCString
  model <- #{peek Metadata, model} ptr >>= maybePeek peekCString
  dateTimeOriginal <- #{peek Metadata, dateTimeOriginal} ptr >>= maybePeek peekCString
  subSecTimeOriginal <- #{peek Metadata, subSecTimeOriginal} ptr >>= maybePeek peekCString
  offsetTimeOriginal <- #{peek Metadata, offsetTimeOriginal} ptr >>= maybePeek peekCString
  dateTimeDigitized <- #{peek Metadata, dateTimeDigitized} ptr >>= maybePeek peekCString
  subSecTimeDigitized <- #{peek Metadata, subSecTimeDigitized} ptr >>= maybePeek peekCString
  offsetTimeDigitized <- #{peek Metadata, offsetTimeDigitized} ptr >>= maybePeek peekCString
  
  gpsLatitudeCount <- fromIntegral <$> (#{peek Metadata, gpsLatitudeCount} ptr :: IO CInt)
  gpsLatitude <- #{peek Metadata, gpsLatitude} ptr >>= maybePeek (peekArray gpsLatitudeCount)
  gpsLatitudeRef <- #{peek Metadata, gpsLatitudeRef} ptr >>= maybePeek peekCString
  
  gpsLongitudeCount <- fromIntegral <$> (#{peek Metadata, gpsLongitudeCount} ptr :: IO CInt)

  gpsLongitude <- #{peek Metadata, gpsLongitude} ptr >>= maybePeek (peekArray gpsLongitudeCount)
  gpsLongitudeRef <- #{peek Metadata, gpsLongitudeRef} ptr >>= maybePeek peekCString
  
  gpsAltitude <- #{peek Metadata, gpsAltitude} ptr >>= maybePeek peek
  gpsAltitudeRef <- fmap fromIntegral <$> (#{peek Metadata, gpsAltitudeRef} ptr >>= maybePeek peek :: IO (Maybe CInt))
  return
    Metadata { dateTimeOriginal = dateTimeOriginal
             , subSecTimeOriginal = subSecTimeOriginal
             , offsetTimeOriginal = offsetTimeOriginal
             , dateTime = dateTime
             , offsetTime = offsetTime
             , subSecTime = subSecTime
             , dateTimeDigitized = dateTimeDigitized
             , subSecTimeDigitized = subSecTimeDigitized
             , offsetTimeDigitized = offsetTimeDigitized
             , orientation = orientation
             , uniqueCameraModel = uniqueCameraModel
             , localizedCameraModel = localizedCameraModel
             , model = model
             , gpsLatitude = gpsLatitude
             , gpsLatitudeRef = gpsLatitudeRef
             , gpsLongitude = gpsLongitude
             , gpsLongitudeRef = gpsLongitudeRef
             , gpsAltitude = gpsAltitude
             , gpsAltitudeRef = gpsAltitudeRef
             }


instance Storable Metadata where
        sizeOf _ = #{size Metadata}
        alignment _  = #{alignment Metadata}
        peek = peekMetadata
        poke = undefined
