{-# LANGUAGE MultiParamTypeClasses #-}

module MappingAndroid where

data PushMessage
data PushNotifBuilder

newtype TrackingId = TrackingId String
newtype NotificationId = NotificationId Int

class BaseNotificationManager where
  createNotificationBuilder :: NotificationId -> TrackingId -> PushNotifBuilder 
  createNotificationBuilder n t = undefined


class WaitDriverApprovalNotificationManager where
 startNotif :: Int -> PushMessage -> String -> IO ()  --Pb: startNotif is not pure! -> Maybe MonadNotification ? 
 startNotif n p m = undefined

-- when do we create a MonadXXX ? 
 
