{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Data.Time.Clock
import Yesod.Form.Bootstrap3

import Import

-- pokeForm :: Form ()
-- pokeForm = renderBootstrap3 BootstrapBasicForm ()

getPokeR :: Text -> Handler Html
getPokeR username = do
    pokerUserId <- requireAuthId
    (Entity pokedUserId _) <- runDB $ getBy404 $ UniqueUser username

    now <- liftIO getCurrentTime 
    let poke = Poke pokerUserId pokedUserId now
    runDB $ insert_ poke

    setMessage $ toHtml $ "Poked " <> username

    redirect $ ProfileR username


getProfileR :: Text -> Handler Html
getProfileR username = 
    generateHtml username Nothing

generateHtml username formM = do
    (Entity userId user) <- runDB $ getBy404 $ UniqueUser username
    let email = maybe "-" id $ userEmail user
    pokes <- runDB $ selectList [PokePokee ==. userId] []
    let pokesW = concatMap renderPoke pokes
    defaultLayout $ do
        setTitle $ toHtml $ username <> " Profile"
        [whamlet|
            <a href="@{HomeR}">
                Homepage
            <h1>
                Profile: #{username}
            <div>
                Email: #{email}
            <h2>
                Poked by:
            <ul>
                ^{pokesW}
        |]

    where
        renderPoke (Entity _ (Poke poker pokee timestamp)) = 
            [whamlet|
                <li>
                    #{show poker}
            |]

