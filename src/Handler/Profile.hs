{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import

getProfileR :: Text -> Handler Html
getProfileR username = do
    (Entity userId user) <- runDB $ getBy404 $ UniqueUser username
    let email = maybe "-" id $ userEmail user
    defaultLayout $ do
        setTitle $ toHtml $ username <> " Profile"
        [whamlet|
            <a href="@{HomeR}">
                Homepage
            <h1>
                Profile: #{username}
            <div>
                Email: #{email}
        |]
