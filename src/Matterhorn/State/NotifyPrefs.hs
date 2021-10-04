{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.NotifyPrefs
    ( enterEditNotifyPrefsMode
    , exitEditNotifyPrefsMode
    )
where

import Prelude ()
import Matterhorn.Prelude

import Network.Mattermost.Types ( ChannelNotifyProps
                                , TeamId
                                , User(..)
                                , UserNotifyProps(..)
                                , Type(..)
                                , channelNotifyPropsMarkUnread
                                , channelNotifyPropsIgnoreChannelMentions
                                , WithDefault(..)
                                , NotifyOption(..)
                                )

import Brick
import Brick.Forms
import Lens.Micro.Platform ( Lens', (.=), lens )

import Matterhorn.Types


muteLens :: Lens' ChannelNotifyProps Bool
muteLens = lens (\props -> props^.channelNotifyPropsMarkUnreadL == IsValue NotifyOptionMention)
           (\props muted -> props { channelNotifyPropsMarkUnread =
                                          if muted
                                          then IsValue NotifyOptionMention
                                          else IsValue NotifyOptionAll
                                  })

channelMentionLens :: Lens' ChannelNotifyProps Bool
channelMentionLens = lens (\props -> props^.channelNotifyPropsIgnoreChannelMentionsL == IsValue True)
                     (\props ignoreChannelMentions ->
                          props { channelNotifyPropsIgnoreChannelMentions = if ignoreChannelMentions
                                                                            then IsValue True
                                                                            else Default
                                })

notifyOptionName :: NotifyOption -> Text
notifyOptionName NotifyOptionAll = "All activity"
notifyOptionName NotifyOptionMention = "Mentions"
notifyOptionName NotifyOptionNone = "Never"

mkNotifyButtons :: ((WithDefault NotifyOption) -> Name)
                -> Lens' ChannelNotifyProps (WithDefault NotifyOption)
                -> NotifyOption
                -> ChannelNotifyProps
                -> FormFieldState ChannelNotifyProps e Name
mkNotifyButtons mkName l globalDefault =
    let optTuple opt = (IsValue opt, mkName $ IsValue opt, notifyOptionName opt)
        defaultField = (Default, mkName Default, "Global default (" <> notifyOptionName globalDefault <> ")")
        nonDefault = optTuple <$> [ NotifyOptionAll
                                  , NotifyOptionMention
                                  , NotifyOptionNone
                                  ]
    in radioField l (defaultField : nonDefault)

notifyPrefsForm :: TeamId -> UserNotifyProps -> ChannelNotifyProps -> Form ChannelNotifyProps e Name
notifyPrefsForm tId globalDefaults =
    newForm [ checkboxField muteLens (MuteToggleField tId) "Mute channel"
            , (padTop $ Pad 1) @@= checkboxField channelMentionLens (ChannelMentionsField tId) "Ignore channel mentions"
            , radioStyle "Desktop notifications" @@=
                mkNotifyButtons (DesktopNotificationsField tId) channelNotifyPropsDesktopL (userNotifyPropsDesktop globalDefaults)
            , radioStyle "Push notifications" @@=
                mkNotifyButtons (PushNotificationsField tId) channelNotifyPropsPushL (userNotifyPropsPush globalDefaults)
            ]
    where radioStyle label = (padTop $ Pad 1 ) . (str label <=>) . (padLeft $ Pad 1)

enterEditNotifyPrefsMode :: MH ()
enterEditNotifyPrefsMode = do
    tId <- use csCurrentTeamId
    chanInfo <- use (csCurrentChannel(tId).ccInfo)
    case chanInfo^.cdType of
      Direct -> mhError $ GenericError "Cannot open notification preferences for DM channel."
      _ -> do
        let props = chanInfo^.cdNotifyProps
        user <- use csMe
        csCurrentTeam.tsNotifyPrefs .= (Just (notifyPrefsForm tId (userNotifyProps user) props))
        setMode tId EditNotifyPrefs

exitEditNotifyPrefsMode :: MH ()
exitEditNotifyPrefsMode = do
    tId <- use csCurrentTeamId
    setMode tId Main
